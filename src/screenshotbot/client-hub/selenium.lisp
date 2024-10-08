(defpackage :screenshotbot/client-hub/selenium
  (:use #:cl)
  (:import-from #:screenshotbot/hub/server
                #:direct-selenium-url
                #:relay-session-request
                #:request-session-and-respond
                #:*hub*)
  (:import-from #:screenshotbot/replay/proxy
                #:replay-proxy)
  (:import-from #:server/interrupts
                #:unwind-on-interrupt)
  (:import-from #:scale/vagrant
                #:vagrant)
  (:import-from #:scale/core
                #:with-cached-ssh-connections
                #:http-request-via
                #:ssh-run
                #:delete-instance
                #:create-instance)
  (:import-from #:scale/image
                #:create-image-instance)
  (:import-from #:screenshotbot/replay/services
                #:firefox)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/client-hub/flags))
  (:export
   #:start-hub))
(in-package :screenshotbot/client-hub/selenium)

(defvar *lock* (bt:make-lock))

(defclass session (store-object)
  ((id :initarg :id
       :reader session-id
       :index-type unique-index
       :index-initargs (:test #'equal)
       :index-reader session-by-id)
   (instance :initarg :instance
             :relaxed-object-reference t
             :accessor session-instance)
   (last-used :initform (get-universal-time)
              :accessor last-used))
  (:metaclass persistent-class))

(defclass client-hub ()
  ((sessions :initform nil
             :accessor sessions)))

(defun find-session (hub session-id)
  (let ((sessions (bt:with-lock-held (*lock*) (sessions hub))))
    (loop for session in sessions
          if (string-equal (session-id session) session-id)
            return session)))

(defmethod touch-session (session)
  (with-transaction ()
   (setf (last-used session) (get-universal-time))))

(defclass vagrant-based-hub (client-hub)
  ())


(defvar *vagrant-service* scale/vagrant:*vagrant*)

(defvar *vagrant-hub* (make-instance 'vagrant-based-hub))

(defmethod request-session-and-respond ((hub client-hub)
                                        (arguments string))
  (error "unimpl"))

(defmethod request-session-and-respond ((hub vagrant-based-hub)
                                        (arguments string))
  (with-cached-ssh-connections ()
   (let ((instance (create-image-instance
                    '(firefox :version "102.0")
                    *vagrant-service*
                    :size :small)))
     (restart-case
         (process-vagrant-instance hub instance arguments)
       (cleanup-image-and-error ()
         (delete-instance instance)
         (error "No instance to work with since we cleaned up the image"))))))

(defmethod direct-selenium-url ((hub client-hub)
                                session-id)
  "http://localhost:4444")

(defmethod relay-session-request ((hub client-hub)
                                  &key method content content-type
                                    script-name)
  (let* ((session-id (elt (str:split "/" script-name) 4))
         (rest-script (a:lastcar (str:split "/" script-name :limit 4)))
         (url (format nil "~a/~a"
                      (direct-selenium-url hub session-id)
                      rest-script)))
    (log:info "Delegating request for session: ~a to ~a" session-id url)
    (let* ((session (find-session hub session-id))
           (instance (session-instance session)))
      (touch-session session)
      (multiple-value-bind (data ret headers)
          (http-request-via instance
                            url
                            :method method
                            :want-string t
                            :content content
                            :content-type content-type)
        ;;(assert (not (eql ret 500)))
        (setf (hunchentoot:return-code*) ret)
        (setf (hunchentoot:content-type*) (a:assoc-value headers :content-type))
        (log:info "Relaying back response: ~a" (str:shorten 80 data))

        (maybe-delete-session hub session rest-script method)

        data))))


(defun maybe-delete-session (hub session rest-script method)
  (log:trace "Got query: ~a ~a" method rest-script)
  (when (and (eql method :delete)
             (eql 2 (length (str:split "/" rest-script))))
    (bt:with-lock-held (*lock*)
      (a:removef (sessions hub) session))
    (log:info "Deleting instance")
    (delete-instance (session-instance session))))

(auto-restart:with-auto-restart ()
  (defun wait-for-driver-ready (instance url)
    (loop while t do
      (let ((response (ignore-errors
                       (http-request-via instance
                                         (format nil "~a/status" url)
                                         :want-string t))))
        (let ((body (ignore-errors
                     (json:decode-json-from-string response))))
          (cond
            ((a:assoc-value (a:assoc-value body :value) :ready)
             (return t))
            (t
             (log:info "driver not ready yet")
             (sleep 0.1))))))))

(auto-restart:with-auto-restart ()
  (defun process-vagrant-instance (hub instance arguments)
    (ssh-run instance "xvfb-run ./geckodriver -vv --binary firefox/firefox < /dev/null > geckodriver_output 2>&1 &")
    (wait-for-driver-ready instance "http://localhost:4444")
    (multiple-value-bind (resp ret headers)
        (http-request-via instance
                          "http://localhost:4444/session"
                          :method :post
                          :content arguments
                          :content-type "application/json"
                          :want-string t)
      (assert (not (eql ret 500)))
      (log:info "Got response: ~a" resp)
      (setf (hunchentoot:return-code*) ret)
      (setf (hunchentoot:content-type*) (a:assoc-value headers :content-type))

      (let* ((resp (json:decode-json-from-string resp))
             (session-id (a:assoc-value (a:assoc-value resp :value) :session-id)))
        (log:info "Got session-id ~a" session-id)
        (assert session-id)
        (bt:with-lock-held (*lock*)
          (push
           (make-instance 'session
                          :id session-id
                          :instance instance)
           (sessions hub))))
      resp)))


(defun start-hub ()
  (log:info "Starting selenium hub on ~a" flags:*port*)
  (let ((proxy
          (make-instance 'replay-proxy
                          :hub *vagrant-hub*
                          :cache-dir (ensure-directories-exist #P"~/screenshotbot/proxy-cache/")
                          :port flags:*port*)))
    (unwind-on-interrupt ()
        (hunchentoot:start proxy)
        (progn
          (log:info "terminating")
          (hunchentoot:stop proxy)))))

;; (progn (setf flags:*port* 9515) (start-hub))

;; (setf hunchentoot:*catch-errors-p* nil)

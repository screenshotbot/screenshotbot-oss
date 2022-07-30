(defpackage :screenshotbot/client-hub/selenium
  (:use #:cl)
  (:import-from #:screenshotbot/hub/server
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
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/client-hub/flags))
  (:export
   #:start-hub))
(in-package :screenshotbot/client-hub/selenium)

(defvar *lock* (bt:make-lock))

(defclass client-hub ()
  ((instances :initform nil
              :accessor instances)))

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

(defmethod relay-session-request ((hub client-hub)
                                  &key method content content-type
                                    script-name)
  (let* ((session-id (elt (str:split "/" script-name) 4))
         (url (format nil "http://localhost:4444/~a"
                      (a:lastcar (str:split "/" script-name :limit 4)))))
    (log:info "Delegating request for session: ~a to ~a" session-id url)
    (let ((instance (a:assoc-value (instances hub) session-id :test #'equal)))
      (assert instance)
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
        data))))


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
          (setf (a:assoc-value (instances hub) session-id :test #'equal)
                instance)))
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

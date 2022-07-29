(defpackage :screenshotbot/client-hub/selenium
  (:use #:cl)
  (:import-from #:screenshotbot/hub/server
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

(defclass vagrant-based-hub ()
  ((instances :initform nil
              :accessor instances)))


(defvar *vagrant-service* (make-instance 'vagrant))

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
     (bt:with-lock-held (*lock*)
       (push instance (instances hub)))
     (restart-case
         (process-vagrant-instance instance arguments)
       (cleanup-image-and-error ()
         (delete-instance instance)
         (error "No instance to work with since we cleaned up the image"))))))

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
  (defun process-vagrant-instance (instance arguments)
    (ssh-run instance "xvfb-run ./geckodriver -vv --binary firefox/firefox < /dev/null > geckdriver_output 2>&1 &")
    (wait-for-driver-ready instance "http://localhost:4444")
    (let ((resp (http-request-via instance
                                  "http://localhost:4444/session"
                                  :method :post
                                  :content arguments
                                  :content-type "application/json"
                                  :want-string t)))
      (error "don't know what to do with: ~a" resp))))


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

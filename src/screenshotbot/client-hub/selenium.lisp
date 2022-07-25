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
  (let ((instance (create-image-instance
                   '(firefox :version "102.0")
                   *vagrant-service*
                   :size :small)))
    (bt:with-lock-held (*lock*)
     (push instance (instances hub)))
    (restart-case
        (process-vagrant-instance instance)
      (cleanup-image-and-error ()
        (delete-instance instance)
        (error "No instance to work with since we cleaned up the image")))))

(auto-restart:with-auto-restart ()
  (defun process-vagrant-instance (instance)
    (error "unimpl")))


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

(defpackage :server/slynk-preparer
  (:use #:cl)
  (:import-from #:server
                #:defmethod
                #:slynk-prepare
                #:slynk-teardown
                #:*slynk-port*
                #:*slynk-loopback-interface*)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:*actual-slynk-port*))
(in-package :server/slynk-preparer)

(defclass slynk-preparer ()
  ())

(setf server:*slynk-preparer* (make-instance 'slynk-preparer))

(defvar *actual-slynk-port* nil)

(defmethod slynk-prepare ((self slynk-preparer))

  (log:info "starting up slynk")
  (slynk-loop))

(defun slynk-loop ()
  (log:info "Using port for slynk: ~a on ~a" *slynk-port* *slynk-loopback-interface*)
  (setf slynk:*loopback-interface* *slynk-loopback-interface*)
  (try-different-ports))

(define-condition no-available-port (error)
  ()
  (:report "Could not open slynk port"))

(defun try-different-ports ()
  (loop for i from 0 to 20
        for port from (parse-integer *slynk-port*)
        do
           (handler-case
               (progn
                 (log:info "Trying port: ~a" port)
                 (slynk:create-server :port port
                                      ;; if non-nil the connection won't be closed
                                      ;; after connecting
                                      :dont-close t)
                 (setf *actual-slynk-port* port)
                 (return t))
             (error (e)
               (log:info "Failed to create slynk on port ~a: ~a"
                         port
                         e)))
        finally
        (error 'no-available-port)))

(defmethod slynk-teardown ((self slynk-preparer))
  (slynk:stop-server *actual-slynk-port*))

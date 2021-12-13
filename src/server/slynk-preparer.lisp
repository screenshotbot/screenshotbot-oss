(defpackage :server/slynk-preparer
  (:use #:cl)
  (:import-from #:server
                #:defmethod
                #:slynk-prepare
                #:slynk-teardown
                #:*slynk-port*
                #:*slynk-loopback-interface*)
  (:local-nicknames (#:a #:alexandria)))
(in-package :server/slynk-preparer)

(defclass slynk-preparer ()
  ())

(setf server:*slynk-preparer* (make-instance 'slynk-preparer))

(defmethod slynk-prepare ((self slynk-preparer))
  (log:info "starting up slynk")
  (slynk-loop))

(defun slynk-loop ()
  (log:info "Using port for slynk: ~a" *slynk-port*)
  (setf slynk:*loopback-interface* *slynk-loopback-interface*)
  (slynk:create-server :port (parse-integer *slynk-port*)
                       ;; if non-nil the connection won't be closed
                       ;; after connecting
                       :dont-close t))

(defmethod slynk-teardown ((self slynk-preparer))
  (slynk:stop-server (parse-integer *slynk-port*)))

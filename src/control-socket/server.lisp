(defpackage :control-socket/server
  (:use #:cl)
  (:import-from #:util/threading
                #:make-thread))
(in-package :control-socket/server)

(defclass control-socket ()
  ((socket :initarg :socket
           :reader socket)
   (thread :initarg :thread
           :accessor thread)))

(define-condition stop-control-socket (condition)
  ())

(defun make-control-socket (pathname)
  (let ((socket (unix-sockets:make-unix-socket pathname)))
    (let ((self (make-instance 'control-socket :socket socket)))
      (setf (thread self)
            (make-thread (lambda ()
                           (unwind-protect
                                (cs-listen self)
                             (delete-file pathname)))))
      self)))

(defmethod control-socket-stop ((self control-socket))
  (bt:interrupt-thread (thread self)
                       (lambda ()
                         (signal 'stop-control-socket))))


(defmethod cs-listen ((self control-socket))
  (handler-case
      (loop
        (let ((socket (unix-sockets:accept-unix-socket (socket self))))
          (make-thread
           (lambda ()
             (dispatch-request self socket))
           :name "control-socket-worker-thread")))
    (stop-control-socket ()
      (log:info "Stopping supervisor")
      (unix-sockets:close-unix-socket (socket self)))))

(defvar *num* 0)

(defmethod dispatch-request ((self control-socket) socket)
  (let ((stream (unix-sockets:unix-socket-stream socket)))
    (dbg:create-client-remote-debugging-connection
     (format nil "Control socket connection ~a" (incf *num*))
     :stream stream
     :log-stream t)))

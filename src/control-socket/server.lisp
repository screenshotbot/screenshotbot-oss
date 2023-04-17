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
                              (cs-listen self))))
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
            (dispatch-request self socket)))))
    (stop-control-socket ()
      (log:info "Stopping supervisor")
      (values))))

(defmethod dispatch-request ((self control-socket) socket)
  (log:info "Got new control-socket connection")
  (unix-sockets:with-unix-socket (socket socket)
    (let ((stream (flex:make-flexi-stream (unix-sockets:unix-socket-stream socket))))
      (let ((cmd (read stream)))
        (log:info "Got command: ~a" cmd)))))

;; (defvar *sock* (make-control-socket "/tmp/foo5"))

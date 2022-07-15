(defpackage :scale/core
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:create-instance
   #:delete-instance
   #:wait-for-ready
   #:with-instance))
(in-package :scale/core)

(defgeneric create-instance (provider type &key region))

(defgeneric delete-instance (instance))

(defgeneric wait-for-ready (instance))

(Defun call-with-instance (fn &rest args)
  (let ((instance (apply #'create-instance args)))
    (unwind-protect
         (progn
           (wait-for-ready instance)
           (funcall fn instance))
      (delete-instance instance))))

(defmacro with-instance ((instance &rest create-instance-args) &body body)
  `(call-with-instance
    (lambda (,instance)
      ,@body)
    ,@create-instance-args))

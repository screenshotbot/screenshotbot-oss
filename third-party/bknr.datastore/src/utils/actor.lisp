(in-package :bknr.utils)

(defclass bknr-actor ()
  ((name :initarg :name :accessor bknr-actor-name)
   (process :accessor bknr-actor-process))
  (:default-initargs :name "anonymous actor"))

(defgeneric run-function (actor))

(defmethod print-object ((actor bknr-actor) stream)
  (format stream 
	  "#<~a \"~a\" (~:[no process~;process running~])>"
	  (class-name (class-of actor))
	  (bknr-actor-name actor)
	  (slot-boundp actor 'process))
  actor)

(defmethod actor-start ((actor bknr-actor))
  (actor-stop actor)
  (setf (slot-value actor 'process)
    (make-process (lambda ()
                    (funcall #'run-function actor))
                  :name (bknr-actor-name actor))))

(defmethod actor-running-p ((actor bknr-actor))
  (and (slot-boundp actor 'process)
       (process-active-p (bknr-actor-process actor))))

(defmethod actor-stop ((actor bknr-actor))
  (when (slot-boundp actor 'process)
    (destroy-process (bknr-actor-process actor))
    (slot-makunbound actor 'process)))

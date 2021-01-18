(eval-when (:compile-toplevel :load-toplevel :execute)
  (or (find-package :sarace)
      (defpackage :sarace (:use :cl :pcl))))
(in-package :sarace)

(defclass bug-metaclass (standard-class)
  ())

(defmethod validate-superclass ((sub bug-metaclass) (super standard-class))
  t)

(defclass bug-direct-slot-definition (standard-direct-slot-definition)
  ())

(defclass bug-effective-slot-definition (standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class bug-metaclass) &rest initargs)
  (declare (ignore initargs))
  'bug-direct-slot-definition)

(defmethod effective-slot-definition-class ((class bug-metaclass) &rest initargs)
  (declare (ignore initargs))
  'bug-effective-slot-definition)

#+nil
(defmethod initialize-instance :after ((class bug-metaclass) &rest initargs)
  (declare (ignore initargs))
  (format t "SLOTS: ~S~%" (class-slots class)))

#+nil
(defmethod reinitialize-instance :around ((class bug-metaclass) &rest initargs)
  (declare (ignore initargs))
  (let* ((old-slots (class-slots class))
	 (new-class (call-next-method))
	 (new-slots (class-slots new-class)))
    (format t "OLD-slots: ~S new-slots ~S~%" old-slots new-slots)
    new-class))

#+nil
(defmethod compute-slots ((class bug-metaclass))
  (let ((normal-slots (call-next-method)))
    (cons (make-instance 'bug-effective-slot-definition
			 :name 'bug-slot
			 :initform nil
			 :class class
			 :initfunction #'(lambda () nil))
	  normal-slots)))

(defmethod (setf slot-value-using-class) :before
    (newvalue (class bug-metaclass) object slot)
  (format t "BEFORE method for slot setting~%"))

(defmethod (setf slot-value-using-class) :around
    (newvalue (class bug-metaclass) object slot)
  (format t "before slot setting~%")
  (let ((result (call-next-method)))
    (format t "after slot setting~%")
    result))

(defmacro with-traced-functions ((&rest functions) &body body)
  `(unwind-protect (progn (trace ,@functions) ,@body)
    (untrace ,@functions)))

;;; BUG BUG with 2 classes, before and around 
  (defclass test ()
    ((a :initarg :a :initform 0))
    (:metaclass bug-metaclass))


(defclass nobug-class (standard-class)
  ())

(defmethod validate-superclass ((sub nobug-class) (super standard-class))
  t)

(defclass nobug-direct-slot-definition (standard-direct-slot-definition)
  ())

(defclass nobug-effective-slot-definition (standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class nobug-class) &rest initargs)
  (declare (ignore initargs))
  'nobug-direct-slot-definition)

(defmethod effective-slot-definition-class ((class nobug-class) &rest initargs)
  (declare (ignore initargs))
  'nobug-effective-slot-definition)

(defmethod (setf slot-value-using-class) :around (newval (class nobug-class) object slot)
  (call-next-method))


(defclass test2 ()
  ((b :initarg :a :initform 0))
  (:metaclass nobug-class))

(in-package :cl-user)
(use-package :aclmop)

;;; compute-slots returns a set of effective slot definitions.

(defun mapappend (function list)
  (let (res)
    (dolist (l list)
      (push (funcall function l) res))
    (nreverse res)))

(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

;;; (setf getf*) is like (setf getf) except that it always changes the
;;; list, which must be non-nil
(defun (setf getf*) (new-value plist key)
  (block body
    (do ((x plist (cddr x)))
	((null x))
      (when (eq (car x) key)
	(setf (car (cdr x)) new-value)
	(return-from body new-value)))
    (push-on-end key plist)
    (push-on-end new-value plist)
    new-value))

(defclass attributes-direct-slot-definition
    (standard-direct-slot-definition)
  ((attributes :initform nil :initarg :attributes
	       :accessor attributes-direct-slot-definition-attributes)))

(defclass attributes-effective-slot-definition
    (standard-effective-slot-definition)
  ((attributes :initform nil :initarg :attributes
	       :accessor attributes-effective-slot-definition-attributes)))

(defclass attributes-class (standard-class)
  ())

(defmethod direct-slot-definition-class ((class attributes-class)
					 &rest initargs)
  'attributes-direct-slot-definition)

(defmethod effective-slot-definition-class ((class attributes-class)
					 &rest initargs)
  'attributes-effective-slot-definition)

(defmethod compute-effective-slot-definition ((class attributes-class) name
					      direct-slots)
  (declare (ignore name))
  (let ((normal-slot (call-next-method)))
    (setf (attributes-effective-slot-definition-attributes normal-slot)
	  (remove-duplicates
	   (mapappend #'attributes-direct-slot-definition-attributes direct-slots)))
    normal-slot))

(defun make-effective-slot-definition (&rest initargs)
  (apply #'make-instance 'standard-effective-slot-definition initargs))

(defmethod compute-slots ((class attributes-class))
  (let* ((normal-slots (call-next-method))
	 (alist (mapcar #'(lambda (slot)
			    (cons (slot-definition-name slot)
				  (mapcar #'(lambda (attr) (cons attr nil))
					  (attributes-effective-slot-definition-attributes
					   slot))))
			normal-slots)))
    (cons (make-effective-slot-definition
	   :name 'all-attributes
	   :initform `',alist
	   :initfunction #'(lambda () alist))
	  normal-slots)))

(defun slot-attribute-bucket (instance slot-name attribute)
  (let* ((all-buckets (slot-value instance 'all-attributes))
	 (slot-bucket (assoc slot-name all-buckets)))
    (unless slot-bucket
      (error "The slot named ~S of ~S has no attributes."
	     slot-name instance))
    (let ((attr-bucket (assoc attribute (cdr slot-bucket))))
      (unless attr-bucket
	(error "The slot named ~S of ~S has no attribute~@
                named ~S." slot-name instance attribute))
      attr-bucket)))

(defun slot-attribute (instance slot-name attribute)
  (cdr (slot-attribute-bucket instance slot-name attribute)))

(defun (setf slot-attribute) (new-value instance slot-name attribute)
  (setf (cdr (slot-attribute-bucket instance slot-name attribute))
	new-value))

;;; test

(defclass credit-rating ()
  ((level :attributes (date-set time-set)))
  (:metaclass attributes-class))


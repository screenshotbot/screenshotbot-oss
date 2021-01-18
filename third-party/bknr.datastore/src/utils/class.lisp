(in-package :bknr.utils)

;;; short form for DEFCLASS

(defun compute-bknr-slot (class slot)
  (destructuring-bind (name access &rest rest) slot
    (let* ((initarg (make-keyword-from-string (symbol-name name)))
	   (accessor (intern (concatenate 'string (symbol-name class) "-"
					  (symbol-name name)) *package*)))
      (unless (getf rest :transient)
        (push initarg rest)
        (push :initarg rest))
      (case access
	(:read
	 (push accessor rest)
	 (push :reader rest))
	(:update
	 (push accessor rest)
	 (push :accessor rest))
	(:none)
	(t (error "unknown access option ~A in slot ~A of class ~A."
		  access slot class)))
      (cons name rest))))

(defmacro define-bknr-class (class (&rest superclasses) slots &rest class-options)
  (let ((slots (mapcar (lambda (slot) (compute-bknr-slot class slot)) slots)))
    ;; the eval-when is there to create the index access functions at compile time
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (defclass ,class ,superclasses
	,slots
	,@class-options))))


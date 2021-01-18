(in-package :bknr.indices)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOS Protocol for index objects

;;; A CLOS cursor is a CLOS class that complies to the following
;;; protocol (note that a cursor doesn't have to support both
;;; CURSOR-NEXT and CURSOR-PREVIOUS):

(defgeneric cursor-next (cursor &optional eoc)
  (:documentation "Return the current value and advance the
cursor. Returns EOC if the cursor is at its end."))

(defgeneric cursor-previous (cursor &optional eoc)
  (:documentation "Return the current value and advance the
cursor. Returns EOC if the cursor is at its end."))

;;; A CLOS index is a CLOS class that complies to the following
;;; protocol (note that an index doesn't have to support all the
;;; protocol methods):

(defgeneric index-add (index object)
  (:documentation "Add OBJECT to the INDEX. Throws an ERROR if a
problem happened while inserting OBJECT."))

(defgeneric index-get (index key)
  (:documentation "Get the object (or the objects) stored under the index-key KEY."))

(defgeneric index-remove (index object)
  (:documentation "Remove OBJECT from the INDEX."))

(defgeneric index-keys (index)
  (:documentation "Returns all the KEYs of the INDEX."))

(defgeneric index-values (index)
  (:documentation "Returns all the objects (or object lists) stored in
INDEX."))

(defgeneric index-values-cursor (index)
  (:documentation "Returns a cursor that walks all the objects stored in INDEX."))

(defgeneric index-keys-cursor (index)
  (:documentation "Returns a cursor that walks all the keys stored in INDEX."))

(defgeneric index-mapvalues (index fun)
  (:documentation "Apply FUN to every object in the INDEX."))

(defgeneric index-reinitialize (new-index old-index)
  (:documentation "Called when the definition of an index is changed."))

(defgeneric index-clear (index)
  (:documentation "Remove all indexed objects from the index."))

(defun index-create (class-name &rest initargs)
  "Instantiate the index object with class CLASS-NAME with INITARGS."
  (apply #'make-instance class-name initargs))

;;; Adding to an index can throw the following errors:

(define-condition index-existing-error (error)
  ((index :initarg :index :reader condition-index)
   (key :initarg :key :reader condition-key)
   (value :initarg :value :reader condition-value)))

(defmethod print-object ((obj index-existing-error) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "INDEX: ~A KEY: ~S VALUE: ~S"
	    (condition-index obj) (condition-key obj) (condition-value obj))))


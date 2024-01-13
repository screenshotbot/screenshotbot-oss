(in-package :bknr.indices)

;; XXX slots from inherited class indices

;;; XXX update-instance-for-different-class
;;; XXX update-instance-for-redefined-class
;;; XXX index-object als toplevel class einfuehren
;;; XXX existierende objekte in die indexe eintrage (geht nicht :( )
;;; ...
;;; restarts mal richtig machen

#+lispworks
(defvar *global-lock* (mp:make-lock :sharing t :recursivep t
                                    :name "BKNR sharing lock")
  "A global lock that will lock all writes while classes/indexes are
being reinitialized")

(defmacro with-sharing-lock (&body body)
  #+lispworks
  `(mp:with-sharing-lock (*global-lock*)
     ,@body)
  #-lispworks
  `(progn ,@body))

(defmacro with-exclusive-lock (&body body)
  #+lispworks
  `(mp:with-exclusive-lock (*global-lock*)
    ,@body)
  #-lispworks
  `(progn ,@body))

(defclass indexed-class (standard-class)
  ((indices :initarg :indices :initform nil
	    :accessor indexed-class-indices)
   (old-indices :initarg :old-indices :initform nil
		:accessor indexed-class-old-indices)
   (index-definitions :initarg :class-indices :initform nil
		      :accessor indexed-class-index-definitions)))

(defstruct index-holder
  class slots name index index-subclasses)

(defmethod indexed-class-index-named ((class indexed-class) index-name)
  (let ((index-holder (find index-name (indexed-class-indices class)
			    :key #'index-holder-name)))
    (when index-holder
      (index-holder-index index-holder))))

(defmethod validate-superclass ((sub indexed-class) (super standard-class))
  t)

(defclass index-direct-slot-definition (standard-direct-slot-definition)
  ((index :initarg :index :initform nil
	  :reader index-direct-slot-definition-index
	  :documentation "Slot keyword for an already existing index")

   (index-var :initarg :index-var :initform nil
	      :reader index-direct-slot-definition-index-var
	      :documentation "Symbol that will be bound to the index")

   (index-type :initarg :index-type :initform nil
	       :reader index-direct-slot-definition-index-type
	       :documentation "Slot keyword to specify the class of a new slot index")
   (index-initargs :initarg :index-initargs :initform nil
	       :reader index-direct-slot-definition-index-initargs
	       :documentation "Arguments that will be passed to
INDEX-CREATE when creating a new slot index")

   (index-reader :initform nil
		 :initarg :index-reader
		 :accessor index-direct-slot-definition-index-reader
		 :documentation "Name of a function that will be created to query the slot index")
   (index-values :initform nil
		 :initarg :index-values
		 :accessor index-direct-slot-definition-index-values
		 :documentation "Name of a function that will be
created to get the values stored in the index")
   (index-mapvalues :initform nil
		    :initarg :index-mapvalues
		    :accessor index-direct-slot-definition-index-mapvalues
		    :documentation "Name of a function that will be
created to map over the values stored in the index")
   (index-keys :initform nil
	       :initarg :index-keys
	       :accessor index-direct-slot-definition-index-keys
	       :documentation "Name of a function that will be created
to get the keys stored in the index")

   (index-subclasses :initarg :index-subclasses :initform t
		     :accessor index-direct-slot-definition-index-subclasses
		     :documentation "Specify if the slot index will
also index subclasses of the class to which the slot belongs, default is T")

   (class :initform nil
	  :accessor index-direct-slot-definition-class)))

(defclass index-effective-slot-definition (standard-effective-slot-definition)
  ((indices :initarg :indices :initform nil
	        :accessor index-effective-slot-definition-indices)
   #+lispworks ;; :class is not an arg for standard-effective-slot-definition
   (%class :initarg :class)))


(defmethod class-all-indexed-superclasses ((class indexed-class))
  (let (result)
    (labels ((superclasses (class)
	       (let ((classes (remove-if-not #'(lambda (class)
						 (typep class 'indexed-class))
					     (class-direct-superclasses class))))
		 (dolist (class classes)
		   (unless (class-finalized-p class)
		     (finalize-inheritance class))
		   (push class result)
		   (superclasses class)))))
      (superclasses class))
    (nreverse result)))

(defmethod direct-slot-definition-class ((class indexed-class) &key index index-type
                                                                 &allow-other-keys)
  (if (or index index-type)
      'index-direct-slot-definition
      (call-next-method)))

(defmethod effective-slot-definition-class ((class indexed-class) &rest initargs)
  (declare (ignore initargs))
  'index-effective-slot-definition)

(defun defun-and-compile (defun)
  (let ((function (second defun)))
    (when function
      (eval
       `(let (#+lispworks (dspec:*redefinition-action* :warn))
          ,defun))
      (compile function))))

(defun create-index-access-functions (index &key index-reader index-values
				                              index-mapvalues index-keys index-var)
  (defun-and-compile
      `(defun ,index-reader (key) (index-get ,index key)))
  (defun-and-compile
      `(defun ,index-values ()
	     (index-values ,index)))
  (defun-and-compile
      `(defun ,index-mapvalues (fun)
	     (index-mapvalues ,index fun)))
  (defun-and-compile
      `(defun ,index-keys ()
	     (index-keys ,index)))
  (when index-var
    (when (boundp index-var)
      (warn "~A is already bound to ~A, rebinding to ~A"
	        index-var (eval index-var) index))
    (eval `(defparameter ,index-var ,index))))

(defun make-index-object (&key index type initargs reader values mapvalues slots keys var)
  (let ((index-object (if index
			              (eval index)
			              (apply #'index-create
				                 (append (cons type (eval-initargs initargs))
					                     (list :slots slots))))))
    (when index-object
      (create-index-access-functions index-object :index-reader reader
				                                  :index-values values
				                                  :index-mapvalues mapvalues
				                                  :index-keys keys
				                                  :index-var var))
    index-object))

(defmethod compute-effective-slot-definition :around ((class indexed-class)
						                              name direct-slots)
  (declare (ignore name))
  (with-exclusive-lock
   (let* ((normal-slot (call-next-method))
	      (direct-slots (remove-if-not #'(lambda (slot)
					                       (typep slot 'index-direct-slot-definition))
				                       direct-slots))
	      (direct-slot (first direct-slots)))
     (when (and (typep normal-slot 'index-effective-slot-definition)
	            direct-slot
	            (or (not (index-direct-slot-definition-class direct-slot))
		            (eql (index-direct-slot-definition-class direct-slot) class)))
       (setf (index-direct-slot-definition-class direct-slot) class)
       (with-slots (index index-type index-initargs index-subclasses index-keys
			        index-reader index-values index-mapvalues index-var) direct-slot
	     (when (or index index-type)
	       (let* ((name (slot-definition-name direct-slot))
		          (index-object (make-index-object :index index
						                           :type index-type
						                           :initargs index-initargs
						                           :reader index-reader
						                           :keys index-keys
						                           :values index-values
						                           :mapvalues index-mapvalues
						                           :var index-var
						                           :slots (list name))))
	         (when index-object
	           (push (make-index-holder :class class :slots (list name)
				                        :name name :index index-object
				                        :index-subclasses index-subclasses)
		             (indexed-class-indices class)))))))
     normal-slot)))

(defmethod compute-class-indices ((class indexed-class) class-indices)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((class-slots (class-slots class))
	 (slot-names (mapcar #'slot-definition-name class-slots)))

    ;;; create new class indices
    (dolist (class-index class-indices)
      #+nil
      (format t "class-index ~A~%" class-index)

      (destructuring-bind (name &key index-reader index-values index-mapvalues
				index-keys (index-subclasses t) index-initargs
				(slots :all-slots) index-type
				index) class-index
	(when (eql slots :all-slots)
	  (setf slots slot-names))

	(let ((index-object (make-index-object :index index
					       :type index-type
					       :initargs index-initargs
					       :reader index-reader
					       :values index-values
					       :keys index-keys
					       :mapvalues index-mapvalues
					       :slots slots)))
	  (when index-object
	    (push (make-index-holder :class class :slots slots
				     :name name :index index-object
				     :index-subclasses index-subclasses)
		  (indexed-class-indices class))))))

    #+nil
    (format t "superclasses ~A~%" (class-all-indexed-superclasses class))

    ;;; class indices from superclasses
    (dolist (superclass (class-all-indexed-superclasses class))
      (setf (indexed-class-indices class)
	    (remove-duplicates
	     (append (indexed-class-indices class)
		     (remove nil (indexed-class-indices superclass)
			     :key #'index-holder-index-subclasses))
	     :key #'index-holder-index)))

    (dolist (holder (indexed-class-indices class))
      (dolist (slot-name (index-holder-slots holder))
	(let ((slot (find slot-name class-slots :key #'slot-definition-name)))
	  #+nil
	  (format t "slot ~A indx ~A~%" slot holder)
	  (unless (and slot
		       (typep slot 'index-effective-slot-definition ))
	    (error "Could not find slot ~A to store index ~A~%" slot-name holder))
	  (pushnew (index-holder-index holder)
		   (index-effective-slot-definition-indices slot)))))))

#+(or allegro lispworks)
(defmethod finalize-inheritance :after ((class indexed-class))
  (with-exclusive-lock
    (compute-class-indices class (indexed-class-index-definitions class))
    (reinitialize-class-indices class)))

(defun validate-index-declaration (class indices)
  (dolist (index indices)
    (when (and (getf (cdr index) :index)
               (getf (cdr index) :index-type))
      (error "Can't have both :INDEX and :INDEX-TYPE in index ~A of ~A" (car index) class))))

(defmethod initialize-instance :before ((class indexed-class) &key class-indices)
  (validate-index-declaration class class-indices))

(defmethod reinitialize-instance :before ((class indexed-class) &key class-indices)
  (validate-index-declaration class class-indices))

;;; avoid late instantiation

#+(or allegro cmu openmcl sbcl lispworks)
(defmethod initialize-instance :after ((class indexed-class) &key)
  (with-exclusive-lock
    (compute-class-indices class (indexed-class-index-definitions class))
    (reinitialize-class-indices class)))

#+(or allegro cmu openmcl sbcl lispworks)
(defmethod reinitialize-instance :after ((class indexed-class) &key)
  (with-exclusive-lock
    (compute-class-indices class (indexed-class-index-definitions class))
    (reinitialize-class-indices class)))

(defmethod reinitialize-class-indices
    ((class indexed-class))
  (let ((old-indices (remove class (indexed-class-old-indices class)
			                 :test-not #'eql :key #'index-holder-class))
	    (indices (remove class (indexed-class-indices class)
			             :test-not #'eql :key #'index-holder-class)))
    (when old-indices
      (dolist (holder indices)
	    (let ((old-holder (find (index-holder-name holder) old-indices
				                :key #'index-holder-name)))
	      (when old-holder
	        (index-reinitialize (index-holder-index holder)
				                (index-holder-index old-holder))))))))

(defmethod reinitialize-instance :before ((class indexed-class) &key)
  (setf (indexed-class-old-indices class) (indexed-class-indices class)
	(indexed-class-indices class) nil))

;;; Hier koennen wir keine :AROUND method fuer COMPUTE-SLOTS bauen,
;;; weil die LISP-Implementierung die Allocation von dem neuen
;;; DESTROYED-P Slot bestimmen muss, und zwar auch im :AROUND. Das
;;; koennen wir leider nicht uebernehmen.

(defmethod compute-slots ((class indexed-class))
  (let* ((normal-slots (call-next-method))
	 (destroyed-p-slot #.`(make-instance
			       'index-effective-slot-definition
			       :name 'destroyed-p
			       :initform nil
			       :class class
			       #+cmu
			       ,@'(:readers nil :writers nil)
			       :initfunction #'(lambda () nil))))
    (cons destroyed-p-slot normal-slots)))

(defvar *indexed-class-override* nil)

(defmethod slot-value-using-class :before ((class indexed-class) object
                                           #-lispworks
                                           slot
                                           #+lispworks
                                           (slot-name symbol))
  #+lispworks
  (assert (symbolp slot-name))
  (let (#-lispworks (slot-name (slot-definition-name slot)))
   (when (and (not (eql slot-name 'destroyed-p))
	          (object-destroyed-p object)
	          (not *indexed-class-override*))
     (error "Can not get slot ~A of destroyed object of class ~a."
	        slot-name (class-name (class-of object))))))

#+lispworks ;; TODO: T393
(defmethod slot-value-using-class ((class indexed-class) object
                                   (slot symbol))
  (call-next-method))

(defmethod (setf slot-value-using-class) :before
    (newvalue (class indexed-class) object
     #-lispworks
     slot
     #+lispworks
     (slot-name symbol))
  (declare (ignore newvalue))
  (let (#-lispworks (slot-name (slot-definition-name slot)))
   (when (and (not (eql slot-name 'destroyed-p))
	          (object-destroyed-p object)
	          (not *indexed-class-override*))
     (error "Can not set slot ~A of destroyed object ~a."
	        slot-name (class-name (class-of object))))))

#+lispworks ;; TODO: T393
(defmethod (setf slot-value-using-class)
    (newvalue (class indexed-class) object (slot symbol))
  (call-next-method))

(defmethod slot-makunbound-using-class :before ((class indexed-class) object
                                                (slot slot-definition))
  (when (and (not (eql (if (symbolp slot)
			   slot
			   (slot-definition-name slot))
		       'destroyed-p))
	     (object-destroyed-p object)
	     (not *indexed-class-override*))
    (error "Can not MAKUNBOUND slot ~A of destroyed object ~a."
	       (slot-definition-name slot) (class-name (class-of object)))))

#+lispworks
(defmethod slot-makunbound-using-class ((class indexed-class) object
                                        (slot symbol))
  (let ((slot-def (clos:find-slot-definition slot class)))
    (unless slot-def
      (error "Did not find slot ~S in ~S"
             slot class))
   (slot-makunbound-using-class class object
                                slot-def)))

(defvar *in-make-instance-p* nil)

(defvar *indices-remove-p* t)

(defmethod make-instance :around ((class indexed-class) &key)
  (with-sharing-lock
   (let* ((*in-make-instance-p* t)
	      (object (call-next-method))
	      (added-indices)
	      (error t))
     (unwind-protect
	      (progn
	        (dolist (index (mapcar #'index-holder-index (indexed-class-indices class)))
	          (index-add index object)
	          (push index added-indices))
	        (setf error nil)
	        object)
       (when error
	     (dolist (index added-indices)
	       (index-remove index object))))
     object)))


(defmethod (setf slot-value-using-class) :around
    (newvalue (class indexed-class) object (slot index-effective-slot-definition))
  (declare (ignore newvalue))

  (with-sharing-lock
    (when (eql (slot-definition-name slot) 'destroyed-p)
      (return-from slot-value-using-class  (call-next-method)))

    (when *in-make-instance-p*
      (return-from slot-value-using-class (call-next-method)))

    (let* ((indices (index-effective-slot-definition-indices slot))
	       (slot-name (slot-definition-name slot))
	       (previous-slot-boundp (slot-boundp object slot-name))
	       (previous-slot-value (when previous-slot-boundp
				                  (slot-value object slot-name))))

      #+nil
      (format t "indices ~A~%" indices)

      (when (and previous-slot-boundp
	             *indices-remove-p*)
        (let ((changed-indices)
	          (error t))
	      (unwind-protect
	           (progn
	             (dolist (index indices)
		           (index-remove index object)
		           (push index changed-indices))
	             (setf error nil))
	        (when error
	          (dolist (index changed-indices)
	            (index-add index object))))))

      (let ((result (call-next-method)))
        #+nil
        (format t "set slot ~A of ~a to ~A, value is ~a~%"
	            (slot-definition-name slot)
	            object newvalue
	            (slot-value object (slot-definition-name slot)))

        (when (slot-boundp object (slot-definition-name slot))
	      (let ((error t)
	            (changed-indices nil))
	        (unwind-protect
	             (progn
		           (dolist (index indices)
		             (index-add index object)
		             (push index changed-indices))
		           (setf error nil))
	          (when error
	            (dolist (index changed-indices)
		          (index-remove index object))
	            (let ((*indices-remove-p* nil))
		          (if previous-slot-boundp
		              (setf (slot-value object slot-name) previous-slot-value)
		              (slot-makunbound object slot-name)))))))
        result))))

(defmethod slot-makunbound-using-class
    ((class indexed-class) object (slot index-effective-slot-definition))
  (let* ((slot-name (slot-definition-name slot))
	 (previous-slot-boundp (slot-boundp object slot-name))
	 (indices (index-effective-slot-definition-indices slot)))
    (when (and previous-slot-boundp
	       *indices-remove-p*)
      (let ((changed-indices nil)
	    (error t))
	(unwind-protect
	     (progn
	       (dolist (index indices)
		 (index-remove index object)
		 (push index changed-indices))
	       (setf error nil))
	  (when error
	    (dolist (index changed-indices)
	      (index-add index object))))))
    (call-next-method)))

(defmethod clear-class-indices ((class indexed-class))
  (map nil #'(lambda (holder) (index-clear (index-holder-index holder)))
       (indexed-class-indices class)))

(defmethod clear-slot-indices ((slot index-effective-slot-definition))
  (map nil #'index-clear (index-effective-slot-definition-indices slot)))

(defmethod class-slot-indices ((class indexed-class) slot-name)
  (index-effective-slot-definition-indices (find slot-name (class-slots class)
                                                 :key #'slot-definition-name)))

(defmethod class-slot-index ((class indexed-class) slot-name)
  (let ((holder (find-if #'(lambda (holder) (and (eql (index-holder-class holder) class)
						                         (eql (index-holder-name holder) slot-name)))
			             (indexed-class-indices class))))
    (when holder
      (index-holder-index holder))))

;;; destroy object mechanic

(defgeneric destroy-object-with-class (class object))
(defgeneric destroy-object (object)
  (:documentation "Destroy the given object, and delete it from the indices."))

(defmethod destroy-object-with-class ((class standard-class) object)
  (declare (ignore object))
  (error "Can not destroy an object that is not indexed."))

(defmethod destroy-object-with-class ((class indexed-class) object)
  (dolist (index (mapcar #'index-holder-index (indexed-class-indices class)))
    (index-remove index object))
  (setf (slot-value object 'destroyed-p) t))

(defmethod destroy-object ((object t))
  (with-sharing-lock
    (destroy-object-with-class (class-of object) object)))

(defmethod object-destroyed-p ((object t))
  (and object
       (slot-boundp object 'destroyed-p)
       (slot-value object 'destroyed-p)))

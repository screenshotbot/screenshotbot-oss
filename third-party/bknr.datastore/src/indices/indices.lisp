;;; XXX protokoll erweitern, das es auch eine funktion gibt, die die
;;; keys aus der klasse extrahiert, dann koennte man viel
;;; zusammenfaktorn

(in-package :bknr.indices)

;;;;;;;;;;;;;;;;;;;;
;;; Slot-bound index

;;; A slot-bound index is based on a hash-table. Objects are added to
;;; the hash-table using the slot-value of the specified slot as key.

(defclass slot-index ()
  ((hash-table :initarg :hash-table :accessor slot-index-hash-table
	       :documentation "The internal hash table used to index
objects.")
   (slot-name :initarg :slot-name :reader slot-index-slot-name
	      :documentation "The value of the slot with name
SLOT-NAME is used as a key to the internal hash-table.")
   (index-nil :initarg :index-nil :reader slot-index-index-nil
	      :initform nil
	      :documentation "If T, NIL is used as a valid slot value, else slots with NIL value are treated as unbound slots.")))

(defmethod initialize-instance :after ((index slot-index) &key (test #'eql) slot-name slots index-nil)
  (unless slots
    (setf slots (list slot-name)))
  (unless (= (length slots) 1)
    (error "Exactly one slot name in :SLOTS initarg required to create a SLOT-INDEX"))
  (with-slots (hash-table slot-name) index
    (setf hash-table (make-hash-table :test test #+sbcl #+sbcl :synchronized t)
	  slot-name (first slots)
	  (slot-value index 'index-nil) index-nil)))

(defmethod print-object ((object slot-index) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "SLOT: ~S SIZE: ~D"
	    (slot-index-slot-name object)
	    (hash-table-count (slot-index-hash-table object)))))

(defmethod index-get ((index slot-index) key)
  (gethash key (slot-index-hash-table index)))

(defmethod index-remove :around ((index slot-index) object)
  (let ((slot-name (slot-index-slot-name index)))
    (if (slot-boundp object slot-name)
	(call-next-method)
	(ignore-errors ;; guard against access to unbound slots in print method
	  (warn "Ignoring request to remove object ~A with unbound slot ~A."
		object slot-name)))))

(defmethod index-remove ((index slot-index) object)
  (remhash (slot-value object (slot-index-slot-name index)) (slot-index-hash-table index)))
  
(defmethod index-keys ((index slot-index))
  (loop for key being the hash-keys of (slot-index-hash-table index)
	collect key))

(defmethod index-values ((index slot-index))
  (loop for value being the hash-values of (slot-index-hash-table index)
	collect value))

(defmethod index-mapvalues ((index slot-index) fun)
  (maphash (lambda (key val) (declare (ignore key)) (funcall fun val))
	   (slot-index-hash-table index)))

(defmethod index-clear ((index slot-index))
  (with-slots (hash-table) index
    (setf hash-table (make-hash-table :test (hash-table-test hash-table) #+sbcl #+sbcl :synchronized t))))

(defmethod index-reinitialize ((new-index slot-index)
			       (old-index slot-index))
  "Reinitialize the slot-bound index from the old index by copying the
internal hash-table if the hash-table test is the same, or by
iterating over the values of the old-table and reentering them into
the new hash-table."
  (let ((new-hash (slot-index-hash-table new-index))
	(old-hash (slot-index-hash-table old-index)))
    (if (eql (hash-table-test new-hash)
	     (hash-table-test old-hash))
	(setf (slot-index-hash-table new-index)
	      old-hash)
	(loop for key being the hash-keys of old-hash using (hash-value value)
	      do (setf (gethash key new-hash) value)))
    new-index))

(defclass unique-index (slot-index)
  ())

(defmethod index-add ((index unique-index) object)
  "Add an object using the value of the specified slot as key. When
the hash-table entry already contains a value, an error is signalled."
  (unless (slot-boundp object (slot-index-slot-name index))
    (return-from index-add))
  (let* ((key (slot-value object (slot-index-slot-name index)))
	 (hash-table (slot-index-hash-table index)))
    (when (and (not (slot-index-index-nil index))
	       (null key))
      (return-from index-add))
    (multiple-value-bind (value presentp)
	(gethash key hash-table)
      (when (and presentp
		 (not (eql value object)))
	(error (make-condition 'index-existing-error
			       :index index :key key :value value)))
      (setf (gethash key hash-table) object))))


(defclass string-unique-index (unique-index)
  ())

(defmethod initialize-instance :after ((index string-unique-index) &key (test #'equal))
  (with-slots (hash-table) index
    (setf hash-table (make-hash-table :test test #+sbcl #+sbcl :synchronized t))))

(defmethod index-add :around ((index string-unique-index) object)
  (unless (slot-boundp object (slot-index-slot-name index))
    (return-from index-add))
  (let* ((key (slot-value object (slot-index-slot-name index))))
    (unless (and (not (slot-index-index-nil index))
		 (string-equal key ""))
      (call-next-method))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slot-bound keyword index

;;; A slot-bound index storing multiple objects under one key. 

(defclass hash-index (slot-index)
  ())

(defmethod index-add ((index hash-index) object)
  (unless (slot-boundp object (slot-index-slot-name index))
    (return-from index-add))
  (let ((key (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index)))
    (when (and (not (slot-index-index-nil index))
	       (null key))
      (return-from index-add))
    (if (nth-value 1 (gethash key hash-table))
        (push object (gethash key hash-table))
        (setf (gethash key hash-table) (list object)))))

(defmethod index-remove ((index hash-index) object)
  (let ((key (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index)))
    (let ((new-value (delete-first object (gethash key hash-table))))
      (if (null new-value)
	  (remhash key hash-table)
	  (setf (gethash key hash-table) new-value)))))

(defmethod index-values ((index hash-index))
  (loop for value being the hash-values of (slot-index-hash-table index)
	appending value))

(defmethod index-mapvalues ((index hash-index) fun)
  (maphash (lambda (key val) (declare (ignore key))
		   (dolist (obj val) (funcall fun obj)))
	   (slot-index-hash-table index)))

;;; Index objects by their class

(defclass class-index (hash-index)
  ((index-superclasses :initarg :index-superclasses :initform nil
		       :reader class-index-index-superclasses)))
  
(defmethod initialize-instance :after ((index class-index) &key index-superclasses)
  (setf (slot-value index 'index-superclasses)
	index-superclasses))

(defmethod index-add ((index class-index) object)
  (labels ((index-object (object class)
	     (let ((key (class-name class))
		   (hash-table (slot-index-hash-table index)))
               (if (nth-value 1 (gethash key hash-table))
                   (push object (gethash key hash-table))
                   (setf (gethash key hash-table) (list object))))))
    
    (if (class-index-index-superclasses index)
	(dolist (class (cons (class-of object)
			     (class-all-indexed-superclasses (class-of object))))
	  (index-object object class))
	(index-object object (class-of object)))))

(defmethod index-remove ((index class-index) object)
    (flet ((remove-object (object class)
	     (let ((key (class-name class))
		   (hash-table (slot-index-hash-table index)))
	       (let ((new-value (delete-first object (gethash key hash-table))))
		 (if (null new-value)
		     (remhash key hash-table)
		     (setf (gethash key hash-table) new-value))))))
      (if (class-index-index-superclasses index)
	  (dolist (class (cons (class-of object)
			       (class-all-indexed-superclasses (class-of object))))
	  (remove-object object class))
	  (remove-object object (class-of object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slot-bound keyword list index

;;; A keyword index, where the slot-value is a list of keys.

(defclass hash-list-index (slot-index)
  ())

(defmethod index-add ((index hash-list-index) object)
  (unless (slot-boundp object (slot-index-slot-name index))
    (return-from index-add))
  (let ((keys (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index)))
    (dolist (key keys)
      (if (nth-value 1 (gethash key hash-table))
          (push object (gethash key hash-table))
          (setf (gethash key hash-table) (list object))))))

(defmethod index-remove ((index hash-list-index) object)
  (let ((keys (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index)))
    (dolist (key keys)
      (let ((new-value (delete-first object (gethash key hash-table))))
	(if (null new-value)
	    (remhash key hash-table)
	    (setf (gethash key hash-table) new-value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multiple-slots array index

(defclass array-index ()
  ((slot-names :initarg :slot-names :accessor array-index-slot-names
	       :initform nil)
   (array :initarg :array :accessor array-index-array)))

(defmethod initialize-instance :after ((index array-index) &key slots dimensions)
  (setf (array-index-array index) (make-array dimensions :initial-element nil)
	(array-index-slot-names index) slots))

(defmethod print-object ((object array-index) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "SLOTS: ~S (~S)"
	    (array-index-slot-names object)
	    (array-dimensions (array-index-array object)))))

(defmethod index-add ((index array-index) object)
  (let* ((slot-values
	  (mapcar #'(lambda (slot-name)
		      ;; return when not all slots are set
		      ;;
		      ;; - 18.10.04 not needed because of
		      ;; make-instance around method
		      ;;
		      ;; - 19.10.04 in fact this is needed because
		      ;; when adding a class index, the existing
		      ;; instances are not reinitailized using
		      ;; make-instnace, so we have to catch this...
		      (unless (slot-boundp object slot-name)
				    (return-from index-add nil))
				  (slot-value object slot-name))
			      (array-index-slot-names index)))
	 (array (array-index-array index))
	 (dimensions (array-dimensions array)))
    (loop for slot-value in slot-values
	  for dimension in dimensions
	  when (>= slot-value dimension)
	  do (error "Could not add ~a to array-index ~a because the coordinates ~a are out of bound." object index slot-values))
    (let ((value (apply #'aref array slot-values)))
      (when (and value
		 (not (eql value object)))
	(error (make-condition 'index-existing-error
			       :index index :key slot-values :value value))))
    (setf (apply #'aref array slot-values)
	  object)))

(defmethod index-get ((index array-index) coords)
  (apply #'aref (array-index-array index) coords))

(defmethod index-remove ((index array-index) object)
  (let* ((slot-values (mapcar #'(lambda (slot-name)
				 ;;; return when not all slots are set
				  (unless (slot-boundp object slot-name)
				    (return-from index-remove nil))
				  (slot-value object slot-name))
			      (array-index-slot-names index)))
	 (array (array-index-array index))
	 (dimensions (array-dimensions array)))
    (loop for slot-value in slot-values
	  for dimension in dimensions
	  when (>= slot-value dimension)
	  do (error "Could not remove ~a from array-index ~a because the coordinates ~a are out of bound." object index slot-values))
    (setf (apply #'aref array slot-values) nil)))

(defmethod index-keys ((index array-index))
  (error "An ARRAY-INDEX has no keys."))

(defmethod index-values ((index array-index))
  (error "An ARRAY-INDEX cannot enumerate its values."))

(defmethod index-mapvalues ((index array-index) (fun function))
  (error "An ARRAY-INDEX cannot enumerate its values."))

(defmethod index-reinitialize ((new-index array-index) (old-index array-index))
  (when (equal (array-dimensions (array-index-array new-index))
	       (array-dimensions (array-index-array old-index)))
    (setf (array-index-array new-index)
	  (array-index-array old-index))
    new-index))

(defmethod index-clear ((index array-index))
  (with-slots (array) index
    (setf array (make-array (array-dimensions array) :initial-element nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ordered skip list index

(defclass skip-list-index ()
  ((skip-list :initarg :skip-list
	      :accessor skip-list-index-skip-list)
   (slot-name :initarg :slot-name
	      :accessor skip-list-index-slot-name)
   (index-nil :initarg :index-nil :initform nil
	      :accessor skip-list-index-index-nil)))

(defmethod initialize-instance :after ((index skip-list-index) &key slots index-nil)
  (unless (<= (length slots) 1)
    (error "Can not create slot-index with more than one slot."))
  (with-slots (skip-list slot-name) index
    (setf skip-list (make-instance 'skip-list)
	  slot-name (first slots)
	  (slot-value index 'index-nil) index-nil)))

(defmethod print-object ((object skip-list-index) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "SLOT: ~S SIZE: ~D"
	    (skip-list-index-slot-name object)
	    (skip-list-length (skip-list-index-skip-list object)))))

(defmethod index-add ((index skip-list-index) object)
  "Add an object using the value of the specified slot as key. When
the hash-table entry already contains a value, an error is thrown."
  (unless (slot-boundp object (skip-list-index-slot-name index))
    (return-from index-add))
  (let* ((key (slot-value object (skip-list-index-slot-name index)))
	 (skip-list (skip-list-index-skip-list index)))
    (when (and (not (skip-list-index-index-nil index))
	       (null key))
      (return-from index-add))
    (let ((value (skip-list-get key skip-list)))
      (when (and value
		 (not (eql value object)))
	(error (make-condition 'index-existing-error
			       :index index :key key :value value)))
      (setf (skip-list-get key skip-list) object))))

(defmethod index-get ((index skip-list-index) key)
  (skip-list-get key (skip-list-index-skip-list index)))

(defmethod index-remove ((index skip-list-index) object)
  (skip-list-delete (slot-value object (skip-list-index-slot-name index))
		    (skip-list-index-skip-list index)))

(defmethod index-keys ((index skip-list-index))
  (let ((keys))
    (map-skip-list #'(lambda (key val) (declare (ignore val))
			     (push key keys))
		   (skip-list-index-skip-list index))
    (nreverse keys)))

(defmethod index-values ((index skip-list-index))
  (let ((vals))
    (map-skip-list #'(lambda (key val) (declare (ignore key))
			     (push val vals))
		   (skip-list-index-skip-list index))
    (nreverse vals)))

(defmethod cursor-next ((slc skip-list-cursor) &optional eoc)
  (declare (ignore eoc))
  (sl-cursor-next slc))

(defmethod cursor-prev ((slc skip-list-cursor) &optional eoc)
  (declare (ignore eoc))
  (sl-cursor-prev slc))

(defmethod index-values-cursor ((index skip-list-index))
  (skip-list-values-cursor (skip-list-index-skip-list index)))

(defmethod index-keys-cursor ((index skip-list-index))
  (skip-list-keys-cursor (skip-list-index-skip-list index)))

(defmethod index-mapvalues ((index skip-list-index) fun)
  (map-skip-list #'(lambda (key val) (declare (ignore key))
			   (funcall fun val))
		 (skip-list-index-skip-list index)))
  
(defmethod index-clear ((index skip-list-index))
  (with-slots (skip-list) index
    (setf skip-list (make-instance 'skip-list))))

(defmethod index-reinitialize ((new-index skip-list-index)
			       (old-index skip-list-index))
  "Reinitialize the slot-bound index from the old index by copying the
internal skip-list if the skip-list order function is the same, or by
iterating over the values of the old-list and reentering them into
the new skip-list."
  (let ((new-list (skip-list-index-skip-list new-index)) 
	(old-list (skip-list-index-skip-list old-index)))
    (setf (skip-list-index-skip-list new-list) old-list)
    new-index))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class skip list index

(defclass class-skip-index ()
  ((index-superclasses :initarg :index-superclasses :initform nil
		       :reader class-skip-index-index-superclasses)
   (slot-name :initarg :slot-name
	      :accessor class-skip-index-slot-name)
   (hash-table :accessor class-skip-index-hash-table)))

(defmethod initialize-instance :after ((index class-skip-index)
                                       &key (test #'eql) slots index-superclasses)
  (unless (<= (length slots) 1)
    (error "Can not create slot-index with more than one slot."))
  (with-slots (hash-table slot-name) index
    (setf hash-table (make-hash-table :test test #+sbcl #+sbcl :synchronized t)
	  slot-name (first slots)
	  (slot-value index 'index-superclasses) index-superclasses)))

(defmethod index-add ((index class-skip-index) object)
  (labels ((index-object (object class)
	     (let ((key (class-name class))
		   (hash-table (class-skip-index-hash-table index))
		   (id-key (slot-value object (class-skip-index-slot-name index))))
	       (multiple-value-bind (skip-list presentp)
		   (gethash key hash-table)
		 (if presentp
		     (setf (skip-list-get id-key skip-list) object)
		     (let ((skip-list
			    (setf (gethash key hash-table)
				  (make-instance 'skip-list))))
		       (setf (skip-list-get id-key skip-list) object)))))))

    (if (class-skip-index-index-superclasses index)
	(dolist (class (cons (class-of object)
			     (class-all-indexed-superclasses (class-of object))))
	  (index-object object class))
	(index-object object (class-of object)))))

(defmethod index-remove ((index class-skip-index) object)
    (flet ((remove-object (object class)
	     (let* ((key (class-name class))
		    (hash-table (class-skip-index-hash-table index))
		    (id-key (slot-value object (class-skip-index-slot-name index)))
		    (skip-list (gethash key hash-table)))
	       (when skip-list
		 (skip-list-remove id-key skip-list)))))
      (if (class-skip-index-index-superclasses index)
	  (dolist (class (cons (class-of object)
			       (class-all-indexed-superclasses (class-of object))))
	    (remove-object object class))
	  (remove-object object (class-of object)))))

(defmethod index-get ((index class-skip-index) key)
  (let* ((hash-table (class-skip-index-hash-table index))
	 (skip-list (gethash key hash-table)))
    (when skip-list
      (let ((res))
	(map-skip-list #'(lambda (key val) (declare (ignore key))
				 (push val res))
		       skip-list)
	(nreverse res)))))

(defun copy-skip-list (skip-list)
  (let ((new-skip-list (make-instance 'skip-list)))
    (map-skip-list #'(lambda (key val)
		       (setf (skip-list-get new-skip-list key) val))
		   skip-list)
    new-skip-list))

(defmethod index-reinitialize ((new-index class-skip-index)
			       (old-index class-skip-index))
  (let* ((new-hash (class-skip-index-hash-table new-index)) 
	 (old-hash (class-skip-index-hash-table old-index)))
    (if (eql (hash-table-test old-hash)
	     (hash-table-test new-hash))
	(setf (class-skip-index-hash-table new-index) old-hash)
	(maphash #'(lambda (key value)
		     (setf (gethash key new-hash) value))
		 old-hash))
    new-index))

(defmethod index-clear ((index class-skip-index))
  (with-slots (hash-table) index
    (setf hash-table (make-hash-table :test (hash-table-test hash-table) #+sbcl #+sbcl :synchronized t))))

(defmethod index-keys ((index class-skip-index))
  (loop for key being the hash-keys of (class-skip-index-hash-table index)
	collect key))

;;; XXX class-skip-index set 2 times for every store-object 
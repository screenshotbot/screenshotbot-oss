;;; MOP based object subsystem for the BKNR datastore

;; Internal slots should have a different slot descriptor class, (setf
;; slot-value-using-class) should only be defined for
;; application-defined slots, not internal slots (like ID, maybe
;; others).

;; get-internal-real-time, get-internal-run-time, get-universal-time
;; need to be shadowed and disallowed.

(in-package :bknr.datastore)

(define-condition inconsistent-slot-persistence-definition (store-error)
  ((class :initarg :class)
   (slot-name :initarg :slot-name))
  (:report (lambda (e stream)
             (with-slots (slot-name class) e
               (format stream "Slot ~A in class ~A declared as both transient and persistent"
                       slot-name class)))))

(define-condition object-subsystem-not-found-in-store (store-error)
  ((store :initarg :store))
  (:report (lambda (e stream)
             (with-slots (store) e
               (format stream "Could not find a store-object-subsystem in the current store ~A" store)))))

(define-condition persistent-slot-modified-outside-of-transaction (store-error)
  ((slot-name :initarg :slot-name)
   (object :initarg :object))
  (:report (lambda (e stream)
             (with-slots (slot-name object) e
               (format stream "Attempt to modify persistent slot ~A of ~A outside of a transaction"
                       slot-name object)))))

(defclass store-object-subsystem ()
  ((next-object-id :initform 0
                   :accessor next-object-id
                   :documentation "Next object ID to assign to a new object")
   (snapshot-threads :initarg :snapshot-threads
                     :initform 4
                     :reader snapshot-threads
                     :documentation "Number of threads to use when snapshotting.")))

(defun store-object-subsystem ()
  (let ((subsystem (find-if (alexandria:rcurry #'typep 'store-object-subsystem)
                            (store-subsystems *store*))))
    (unless subsystem
      (error 'object-subsystem-not-found-in-store :store *store*))
    subsystem))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (finalize-inheritance
   (defclass persistent-class (indexed-class)
     ())))

(defmethod validate-superclass ((sub persistent-class) (super indexed-class))
  t)

(defvar *suppress-schema-warnings* nil)

(defun update-instances-for-changed-class (class)
  (let ((instance-count (length (class-instances class))))
    (when (plusp instance-count)
      (unless *suppress-schema-warnings*
        (report-progress "~&; updating ~A instances of ~A for class changes~%"
                         instance-count class))
      (mapc #'reinitialize-instance (class-instances class)))))

(defvar *wait-for-tx-p* t
  "If true, we wait for the transaction to apply before proceeding. This
is only applicable for bknr.cluster.")

(defmethod reinitialize-instance :after ((class persistent-class) &key)
  (when (and (boundp '*store*) *store*)
    (let (#+lispworks (*wait-for-tx-p* nil))
      ;; At least on Lispworks, we might be inside here when the
      ;; entire process is blocked, which means any transactions being
      ;; applied on another thread will never run.
      (update-instances-for-changed-class (class-name class)))
    (unless *suppress-schema-warnings*
      (report-progress "~&; class ~A has been changed. To ensure correct schema ~
                              evolution, please snapshot your datastore.~%"
                       (class-name class)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defclass persistent-direct-slot-definition (index-direct-slot-definition)
   ((relaxed-object-reference :initarg :relaxed-object-reference
                              :initform nil)
    (transient :initarg :transient
               :initform nil)))

 (defclass persistent-effective-slot-definition (index-effective-slot-definition)
  ((relaxed-object-reference :initarg :relaxed-object-reference
                             :initform nil)
   (transient :initarg :transient
              :initform nil)))

 (defgeneric transient-slot-p (slotd)
  (:method ((slotd t))
    t)
  (:method ((slotd persistent-direct-slot-definition))
    (slot-value slotd 'transient))
  (:method ((slotd persistent-effective-slot-definition))
    (slot-value slotd 'transient)))

 (defgeneric relaxed-object-reference-slot-p (slotd)
  (:method ((slotd t))
    nil)
  (:method ((slotd persistent-effective-slot-definition))
    (slot-value slotd 'relaxed-object-reference))
  (:method ((slotd persistent-direct-slot-definition))
    (slot-value slotd 'relaxed-object-reference))
  (:documentation "Return whether the given slot definition specifies
that the slot is relaxed.  If a relaxed slot holds a pointer to
another persistent object and the pointed-to object is deleted, slot
reads will return nil.")))



(defun undo-set-slot (object slot-name value)
  (if (eq value 'unbound)
      (slot-makunbound object slot-name)
      (setf (slot-value object slot-name) value)))

(defmethod (setf slot-value-using-class) :around ((newval t)
                                                  (class persistent-class)
                                                  object
                                                  (slotd persistent-effective-slot-definition))
  (let ((slot-name (slot-definition-name slotd)))
    (cond
      ((or
        (in-transaction-p)
        (transient-slot-p slotd)
        (member slot-name '(last-change id))
        ;; If we're restoring then we don't need to create a new transaction
        (eq :restore (store-state *store*)))
       (call-next-method))
     (t
      ;; If we're not in a transaction, or this is not a transient
      ;; slot, we don't make the change directly, instead we just
      ;; create a transaction for it. The transaction should call
      ;; slot-value-using-class again.
      (execute (make-instance 'transaction
                              :function-symbol 'tx-change-slot-values
                              :timestamp (get-universal-time)
                              :args (list object (slot-definition-name slotd) newval)))
      newval))))


(define-condition transient-slot-cannot-have-initarg (store-error)
  ((class :initarg :class)
   (slot-name :initarg :slot-name))
  (:documentation "A transient slot may not have an :initarg
  specified, as initialize-instance is only used for persistent
  initialization.")
  (:report (lambda (e stream)
             (with-slots (class slot-name) e
               (format stream "The transient slot ~A in class ~A was defined as having an initarg, which is not supported"
                       slot-name (class-name class))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defmethod direct-slot-definition-class ((class persistent-class) &key initargs transient name &allow-other-keys)
   ;; It might be better to do the error checking in an
   ;; initialize-instance method of persistent-direct-slot-definition
   (when (and initargs transient)
     (error 'transient-slot-cannot-have-initarg :class class  :slot-name name))
   'persistent-direct-slot-definition))

(defmethod effective-slot-definition-class ((class persistent-class) &key &allow-other-keys)
  'persistent-effective-slot-definition)

(defmethod compute-effective-slot-definition :around ((class persistent-class) name direct-slots)
  (unless (or (every #'transient-slot-p direct-slots)
              (notany #'transient-slot-p direct-slots))
    (error 'inconsistent-slot-persistence-definition :class class :slot-name name))
  (let ((effective-slot-definition (call-next-method)))
    (when (typep effective-slot-definition 'persistent-effective-slot-definition)
      (with-slots (relaxed-object-reference transient) effective-slot-definition
        (setf relaxed-object-reference (some #'relaxed-object-reference-slot-p direct-slots)
              transient (slot-value (first direct-slots) 'transient))))
    effective-slot-definition))

(defmethod class-persistent-slots ((class standard-class))
  (remove-if #'transient-slot-p (class-slots class)))

(defclass store-object ()
  ((id :initarg :id
       :reader store-object-id
       :type integer
       :index-type unique-index
       :index-initargs (:test #'eql)
       :index-reader store-object-with-id :index-values all-store-objects
       :index-mapvalues map-store-objects)
   (last-change :initform (get-universal-time)
                :initarg :last-change))
  (:metaclass persistent-class)
  (:class-indices (all-class :index-type class-skip-index
                             :index-subclasses t
                             :index-initargs (:index-superclasses t)
                             :index-keys all-store-classes
                             :index-reader store-objects-with-class
                             :slots (id))))

(defun class-instances (class)
  (find-class class)                 ; make sure that the class exists
  (store-objects-with-class class))

(deftransaction store-object-touch (object)
  "Update the LAST-CHANGE slot to reflect the current transaction timestamp."
  (setf (slot-value object 'last-change) (current-transaction-timestamp)))

(defgeneric store-object-last-change (object depth)
  (:documentation "Return the last change time of the OBJECT.  DEPTH
  determines how deep the object graph will be traversed.")

  (:method ((object t) (depth integer))
    0)

  (:method ((object store-object) (depth (eql 0)))
    (slot-value object 'last-change))

  (:method ((object store-object) depth)
    (let ((last-change (slot-value object 'last-change)))
      (dolist (slotd (class-slots (class-of object)))
        (let* ((slot-name (slot-definition-name slotd))
               (child (and (slot-boundp object slot-name)
                           (slot-value object slot-name))))
          (setf last-change
                (cond
                  ((null child)
                   last-change)
                  ((typep child 'store-object)
                   (max last-change (store-object-last-change child (1- depth))))
                  ((listp child)
                   (reduce #'max child
                           :key (alexandria:rcurry 'store-object-last-change (1- depth))
                           :initial-value last-change))
                  (t
                   last-change)))))
      last-change)))

#+allegro
(aclmop::finalize-inheritance (find-class 'store-object))

(defmethod fix-make-instance-args ((class persistent-class)
                                   initargs)
  (list*
   class
   (append
    initargs
    (let ((used-keys (loop for key in initargs by #'cddr
                           collect key)))
      (loop for initarg in (closer-mop:class-default-initargs class)
            unless (member (first initarg)
                           used-keys)
              appending (list
                         (first initarg)
                         (funcall (third initarg))))))))

(defmethod make-instance :around ((class persistent-class) &rest initargs
                                  &key
                                    id)
  "There are three ways make-instance can be called:

   * Outside of a transaction, in which case we have to encode a
     transaction, but also use default-initargs

   * Inside a deftransaction, in which case we use default-initargs
     immediately, but don't encode a transaction

   * When commiting or replaying a transaction. As with the last
     case (in-transaction-p) will be true here, but :id will truthy
     since we have already processed default-initargs.
"
  (cond
    ((or
      (in-transaction-p)
      (eq :restore (store-state *store*)))
     (cond
       (id
        (call-next-method))
       (t
        (apply #'tx-make-instance
               (fix-make-instance-args class initargs)))))
    (t
     (with-store-guard ()
       (let ((transaction
               (destructuring-bind (class . args)
                   (fix-make-instance-args
                    class initargs)
                (make-instance 'transaction
                               :function-symbol 'tx-make-instance
                               :timestamp (get-universal-time)
                               :args (list*
                                      (class-name class)
                                      args)))))
         (execute transaction))))))

(defun tx-make-instance (class &rest args)
  ;; class might either be a symbol, or a class depending on whether
  ;; this was a toplevel transaction or called inside another
  ;; transaction.
  (apply #'make-instance class
         :id (allocate-next-object-id)
         args))

(defvar *allocate-object-id-lock* (bt:make-lock "Persistent Object ID Creation"))

(defun allocate-next-object-id ()
  (mp-with-lock-held (*allocate-object-id-lock*)
    (let ((id (next-object-id (store-object-subsystem))))
      (incf (next-object-id (store-object-subsystem)))
      id)))

(defun initialize-transient-slots (object)
  (dolist (slotd (class-slots (class-of object)))
    (when (and (typep slotd 'persistent-effective-slot-definition)
               (transient-slot-p slotd)
               (slot-definition-initfunction slotd))
      (setf (slot-value object (slot-definition-name slotd))
            (funcall (slot-definition-initfunction slotd))))))

(defmethod initialize-instance :after ((object store-object) &key)
  ;; This is called only when initially creating the (persistent)
  ;; instance, not during restore.  During restore, the
  ;; INITIALIZE-TRANSIENT-INSTANCE function is called for all
  ;; persistent objects after the snapshot has been read, but before
  ;; running the transaction log.
  (initialize-transient-instance object))

(defmacro print-store-object ((object stream &key type) &body body)
  ;; variable capture accepted here.
  `(print-unreadable-object (,object ,stream :type ,type)
     (format stream "ID: ~D " (store-object-id ,object))
     ,@body))

(defmethod print-object ((object store-object) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "ID: ~D" (store-object-id object))))

(defmethod print-object :around ((object store-object) stream)
  (if (object-destroyed-p object)
      (print-unreadable-object (object stream :type t)
        (princ "DESTROYED" stream))
      (call-next-method)))

(defmethod change-class :before ((object store-object) class &rest args)
  (declare (ignore class args))
  (when (not (in-transaction-p))
    (error "Can't change class of persistent object ~A using change-class ~
            outside of transaction, please use PERSISTENT-CHANGE-CLASS instead" object)))

(defun tx-persistent-change-class (object class-name &rest args)
  (warn "TX-PERSISTENT-CHANGE-CLASS does not maintain class indices, ~
         please snapshot and restore to recover indices")
  (apply #'change-class object (find-class class-name) args))

(defun persistent-change-class (object class &rest args)
  (execute (make-instance 'transaction :function-symbol 'tx-persistent-change-class
                          :timestamp (get-universal-time)
                          :args (append (list object (if (symbolp class) class (class-name class))) args))))

(defgeneric initialize-transient-instance (store-object)
  (:documentation
   "Initializes the transient aspects of a persistent object. This
method is called after a persistent object has been initialized, also
when the object is loaded from a snapshot, but before reading the
transaction log."))

(defmethod initialize-transient-instance ((object store-object)))

(defmethod store-object-persistent-slots ((object store-object))
  (mapcar #'slot-definition-name (class-persistent-slots (class-of object))))

(defmethod store-object-relaxed-object-reference-p ((object store-object) slot-name)
  (let ((slot (find slot-name (class-slots (class-of object)) :key #'slot-definition-name)))
    (when slot
      (relaxed-object-reference-slot-p slot))))

(defmacro define-persistent-class (class (&rest superclasses) slots &rest class-options)
  (let ((superclasses (or superclasses '(store-object)))
        (metaclass (cadr (assoc :metaclass class-options))))
    (when (and metaclass
               (not (validate-superclass (find-class metaclass)
                                         (find-class 'persistent-class))))
      (error "Can not define a persistent class with metaclass ~A." metaclass))
    `(define-bknr-class ,class ,superclasses ,slots
                        ,@(unless metaclass '((:metaclass persistent-class)))
                        ,@class-options)))

(defmacro defpersistent-class (class (&rest superclasses) slots &rest class-options)
  (let ((superclasses (or superclasses '(store-object)))
        (metaclass (cadr (assoc :metaclass class-options))))
    (when (and metaclass
               (not (validate-superclass (find-class metaclass)
                                         (find-class 'persistent-class))))
      (error "Can not define a persistent class with metaclass ~A." metaclass))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,class ,superclasses ,slots
         ,@(unless metaclass '((:metaclass persistent-class)))
         ,@class-options))))

;;; binary snapshot

(defvar *current-object-slot* nil)
(defvar *current-slot-relaxed-p* nil)

(defun encode-layout (id class slots stream)
  (%write-tag #\L stream)
  (%encode-integer id stream)
  (%encode-symbol (class-name class) stream)
  (%encode-integer (length slots) stream)
  (dolist (slot slots)
    (%encode-symbol slot stream)))

(defun %encode-set-slots (slots object stream)
  (dolist (slot slots)
    (let ((*current-object-slot* (list object slot))
          (*current-slot-relaxed-p* (store-object-relaxed-object-reference-p object slot)))
      (encode (if (slot-boundp object slot)
                  (slot-value object slot)
                  'unbound)
              stream))))

(defun encode-create-object (class-layouts object stream)
  (let* ((class (class-of object))
         (layout (gethash class class-layouts)))
    (unless layout
      (setf layout
            (cons (hash-table-count class-layouts)
                  ;; XXX layout muss konstant sein
                  (sort (remove 'id (store-object-persistent-slots object))
                        #'string< :key #'symbol-name)))
      (encode-layout (car layout) class (cdr layout) stream)
      (setf (gethash class class-layouts) layout))
    (destructuring-bind (layout-id &rest slots) layout
      (declare (ignore slots))
      (%write-tag #\O stream)
      (%encode-integer layout-id stream)
      (%encode-integer (store-object-id object) stream))))

(defun encode-set-slots (class-layouts object stream)
  (destructuring-bind (layout-id &rest slots)
      (gethash (class-of object) class-layouts)
    (%write-tag #\S stream)
    (%encode-integer layout-id stream)
    (%encode-integer (store-object-id object) stream)
    (%encode-set-slots slots object stream)))

(defun find-class-with-interactive-renaming (class-name)
  (loop until (or (null class-name)
                  (find-class class-name nil))
     do (progn
          (format *query-io* "Class ~A not found, enter new class or enter ~
                              NIL to ignore objects of this class: "
                  class-name)
          (finish-output *query-io*)
          (setq class-name (read *query-io*))))
  (and class-name
       (find-class class-name)))

(defun find-slot-name-with-interactive-rename (class slot-name)
  (loop until (find slot-name (class-slots class) :key #'slot-definition-name)
     do (format *query-io* "Slot ~S not found in class ~S, enter new slot name: "
                slot-name (class-name class))
     do (setq slot-name (read *query-io*))
     finally (return slot-name)))

(defvar *slot-name-map*)

(defun rename-slot (class slot-name)
  (or (caddr (find (list (class-name class) slot-name) *slot-name-map*
                   :key #'(lambda (entry) (subseq entry 0 2)) :test #'equal))
      (find (symbol-name slot-name)
            (mapcar #'slot-definition-name (class-slots class)) :key #'symbol-name :test #'equal)))

(defgeneric convert-slot-value-while-restoring (object slot-name value)
  (:documentation "Generic function to be called to convert a slot's
  value from a previous snapshot layout.  OBJECT is the object that is
  being restored, SLOT-NAME is the name of the slot in the old schema,
  VALUE is the value of the slot in the old schema.")
  (:method (object slot-name value)
    (setf (slot-value object slot-name) value)))

(defun find-slot-name-with-automatic-rename (class slot-name)
  (if (find slot-name (class-slots class) :key #'slot-definition-name)
      slot-name
      (restart-case
          (let ((new-slot-name (rename-slot class slot-name)))
            (cond
              (new-slot-name
               (warn "slot ~S not found in class ~S, automatically renamed to ~S"
                     slot-name (class-name class) new-slot-name)
               new-slot-name)
              (t
               (error "can't find a slot in class ~A which matches the name ~A used in the store snapshot"
                      (class-name class) slot-name))))
        (convert-values ()
          :report "Convert slot values using CONVERT-SLOT-VALUE-WHILE-RESTORING"
          (cons 'convert-slot-values slot-name))
        (ignore-slot ()
          :report "Ignore slot, discarding values found in the snapshot file"
          nil))))

(defun find-class-slots-with-interactive-renaming (class slot-names)
  #+(or)
  (format t "; verifying class layout for class ~A~%; slots:~{ ~S~}~%" (class-name class)
          (mapcar #'slot-definition-name (class-slots class)))
  (loop for slot-name in slot-names
     collect (find-slot-name-with-automatic-rename class slot-name)))

(defun snapshot-read-layout (stream layouts)
  (let* ((id (%decode-integer stream))
         (class-name (%decode-symbol stream :usage "class"))
         (nslots (%decode-integer stream))
         (class (find-class-with-interactive-renaming class-name))
         (slot-names (loop repeat nslots collect (%decode-symbol stream
                                                                 :intern (not (null class))
                                                                 :usage "slot")))
         (slots (if class
                    (find-class-slots-with-interactive-renaming class slot-names)
                    slot-names)))
    (setf (gethash id layouts)
          (cons class slots))))

(defun %read-slots (stream object slots)
  "Read the OBJECT from STREAM.  The individual slots of the object
are expected in the order of the list SLOTS.  If the OBJECT is NIL,
the slots are read from the snapshot and ignored."
  (declare (optimize (speed 3)))
  (dolist (slot-name slots)
    (let ((value (decode stream)))
      (cond
        ((consp slot-name)
         (assert (eq 'convert-slot-values (car slot-name)))
         (convert-slot-value-while-restoring object (cdr slot-name) value))
        ((null slot-name)
         ;; ignore value
         )
        (t
         (restart-case
             (let ((*current-object-slot* (list object slot-name))
                   (*current-slot-relaxed-p* (or (null object)
                                                 (store-object-relaxed-object-reference-p object slot-name))))
               (when object
                 (let ((bknr.indices::*indices-remove-p* nil))
                   (if (eq value 'unbound)
                       (slot-makunbound object slot-name)
                       (convert-slot-value-while-restoring object slot-name value)))))
           (set-slot-nil ()
             :report "Set slot to NIL."
             (setf (slot-value object slot-name) nil))
           (make-slot-unbound ()
             :report "Make slot unbound."
             (slot-makunbound object slot-name))))))))

(defun snapshot-read-object (stream layouts)
  (declare (optimize (speed 3)))
  (with-simple-restart (skip-object "Skip the object.")
    (let* ((layout-id (%decode-integer stream))
           (object-id (%decode-integer stream))
           (class (first (gethash layout-id layouts))))
      ;; If the class is NIL, it was not found in the currently
      ;; running Lisp image and objects of this class will be ignored.
      (when class
        (let ((object (allocate-instance class)))
          (setf (slot-value object 'id) object-id
                (next-object-id (store-object-subsystem)) (max (1+ object-id)
                                                               (next-object-id (store-object-subsystem))))
          (dolist (index (class-slot-indices class 'id))
            (index-add index object)))))))

(defun snapshot-read-slots (stream layouts)
  (let* ((layout-id (%decode-integer stream))
         (object-id (%decode-integer stream))
         (object (store-object-with-id object-id)))
    (restart-case
        (%read-slots stream object (cdr (gethash layout-id layouts)))
      (skip-object-initialization ()
        :report "Skip object initialization.")
      (delete-object ()
        :report "Delete the object."
        (delete-object object)))))

(define-condition encoding-destroyed-object (error)
  ((id :initarg :id)
   (slot :initarg :slot)
   (container :initarg :container))
  (:report (lambda (e out)
             (with-slots (slot container id) e
               (format out
                       "Encoding reference to destroyed object with ID ~A from slot ~A of object ~A with ID ~A."
                       id slot (type-of container) (store-object-id container))))))

(defmethod encode-object ((object store-object) stream)
  (if (object-destroyed-p object)
      (let* ((*indexed-class-override* t)
             (id (store-object-id object))
             (container (first *current-object-slot*))
             (slot (second *current-object-slot*)))

        ;; if we are not encoding slot values, something has gone
        ;; wrong with the indices
        (unless (and container slot)
          (warn "Encoding destroyed object with ID ~A." id)
          (%write-tag #\o stream)
          (%encode-integer id stream)
          (return-from encode-object))

        (flet ((encode-relaxed ()
                 (warn "Encoding reference to destroyed object with ID ~A from slot ~A of object ~A with ID ~A."
                       id slot (type-of container) (store-object-id container))
                 (%write-tag #\o stream)
                 (%encode-integer id stream)))
          (if *current-slot-relaxed-p*
             ;; the slot can contain references to deleted objects, just warn
              (encode-relaxed)
              ;; the slot can't contain references to deleted objects, throw an error
              (restart-case
                  (error 'encoding-destroyed-object
                         :id id
                         :slot slot
                         :container container)
                (encode-relaxed-slot ()
                  (encode-relaxed))
                (make-slot-unbound-and-encode-relaxed ()
                  (encode-relaxed)
                  (slot-makunbound container slot))))))

      ;; Go ahead and serialize the object reference
      (progn (%write-tag #\o stream)
             (%encode-integer (store-object-id object) stream))))

(defmethod decode-object ((tag (eql #\o)) stream)
  (let ((*current-object-slot* nil))
    (%decode-store-object stream)))

(define-condition invalid-reference (warning)
  ((id :initarg :id))
  (:report (lambda (e stream)
             (format stream "internal inconsistency during restore - store object with ID ~A could not be found"
                     (slot-value e 'id)))))

(defun %decode-store-object (stream)
  ;; This is actually called in two contexts, when a slot-value is to
  ;; be filled with a reference to a store object and when a list of
  ;; store objects is read from the transaction log (%decode-list).
  ;; In the former case, references two deleted objects are accepted
  ;; when the slot pointing to the object is marked as being a
  ;; "relaxed-object-reference", in the latter case, no such
  ;; information is available.  To ensure maximum restorability of
  ;; transaction logs, object references stored in lists are always
  ;; considered to be relaxed references, which means that references
  ;; to deleted objects are restored as NIL.  Applications must be
  ;; prepared to cope with NIL entries in such object lists (usually
  ;; lists in slots).
  (let* ((id (%decode-integer stream))
         (object (or (store-object-with-id id)
                     (warn 'invalid-reference :id id)))
         (container (first *current-object-slot*))
         (slot-name (second *current-object-slot*)))
    (cond (object object)

          ((or *current-slot-relaxed-p* (not container))
           (if container
               (warn "Reference to inexistent object with id ~A in relaxed slot ~A of object ~
                      with class ~A with ID ~A."
                     id slot-name (type-of container) (store-object-id container))
               (warn "Reference to inexistent object with id ~A from unnamed container, returning NIL." id))

           ;; Possibly determine new "current object id"
           (when (>= id (next-object-id (store-object-subsystem)))
             (setf (next-object-id (store-object-subsystem)) (1+ id)))
           nil)

          (t (error "Reference to inexistent object with id ~A from slot ~A of object ~A with ID ~A."
                    id slot-name (type-of container)
                    (if container (store-object-id container) "unknown object"))))))

(defun encode-current-object-id (stream)
  (%write-tag #\I stream)
  (%encode-integer (next-object-id (store-object-subsystem)) stream))

(defmethod snapshot-subsystem-async ((store store) (subsystem store-object-subsystem))
  (let ((snapshot-pathname (store-subsystem-snapshot-pathname store subsystem)))
    (snapshot-subsystem-helper subsystem snapshot-pathname)))

(defmethod snapshot-subsystem ((store store) (subsystem store-object-subsystem))
  (error "Unimplemented: call snapshot-subsystem-async instead!"))

(defun snapshot-subsystem-helper (subsystem snapshot-pathname
                                  &key (map-store-objects #'map-store-objects))
  (let ((class-layouts (make-hash-table)))
    (with-open-file (stream snapshot-pathname
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (with-transaction (:prepare-for-snapshot)
        (funcall map-store-objects #'prepare-for-snapshot))
      (encode-current-object-id stream)
      (funcall map-store-objects
               (lambda (object)
                 (when (subtypep (type-of object) 'store-object)
                   (encode-create-object class-layouts object stream)))))

    (let ((objects))
      (funcall map-store-objects
               (lambda (object)
                 (when (subtypep (type-of object) 'store-object)
                   (push object objects))))

      ;; Will return a lambda!
      (encode-object-slots subsystem class-layouts (reverse objects) snapshot-pathname))))

(defun %log-crash (e)
  (format t "Error in background snapshot thread: ~a~%" e)
  #+lispworks
  (dbg:output-backtrace :brief t)
  #-lispworks
  (trivial-backtrace:print-backtrace e))

(defun safe-make-thread (fn)
  (bt:make-thread
   (lambda ()
     (ignore-errors
      (handler-bind ((error #'%log-crash))
        (funcall fn))))))

(defun encode-object-slots (subsystem class-layouts objects snapshot-pathname)
  (labels ((make-batches (objects batch-size)
             (if (<= (length objects) batch-size)
                 (list objects)
                 (list*
                  (subseq objects 0 batch-size)
                  (make-batches
                   (subseq objects batch-size)
                   batch-size)))))
    (let* ((batch-size (ceiling (length objects) (snapshot-threads subsystem)))
           (batches (make-batches objects batch-size))
           (streams (loop for nil in batches
                          collect (uiop:with-temporary-file (:pathname p)
                                    (open p :direction :io
                                            :if-exists :supersede
                                            :element-type '(unsigned-byte 8)))))
           (lock (bt:make-lock))
           (count 0))

      (flet ((close-streams ()
               "Close all the temporary streams that we've opened"
               (mapc #'close streams)))
        (unwind-protect
             (let ((threads
                     (loop for batch in batches
                           for s in streams
                           collect
                           (let ((s s)
                                 (batch batch))
                             (safe-make-thread
                              (lambda ()
                                (loop for object in batch
                                     do (encode-set-slots class-layouts object s))
                                (bt:with-lock-held (lock)
                                  (incf count))))))))
               (mapc #'bt:join-thread threads)

               (unless (= count (length threads))
                 (close-streams)
                 (error "Some threads failed to complete"))

               ;; Finally combine all the streams together
               (lambda ()
                 (unwind-protect
                      (with-open-file (stream snapshot-pathname
                                              :direction :output
                                              :element-type '(unsigned-byte 8)
                                              :if-exists :append)
                        (loop for s in streams do
                          (file-position s 0)
                          (uiop:copy-stream-to-stream s stream :element-type '(unsigned-byte 8)))
                        (finish-output stream))
                   (close-streams)))))))))

(defmethod close-subsystem ((store store) (subsystem store-object-subsystem))
  (dolist (class-name (all-store-classes))
    (clear-class-indices (find-class class-name))))

(defmethod restore-subsystem ((store store) (subsystem store-object-subsystem) &key until)
  ;; XXX check that until > snapshot time
  (declare (ignore until))
  (let ((snapshot (store-subsystem-snapshot-pathname store subsystem)))
    ;; not all indices that should be cleared are cleared. maybe
    ;; check on first instatiation of a class?
    (dolist (class-name (cons 'store-object (all-store-classes)))
      (clear-class-indices (find-class class-name)))
    (setf (next-object-id subsystem) 0)
    (when (probe-file snapshot)
      (report-progress "~&; loading snapshot file ~A~%" snapshot)
      (with-open-file (s snapshot
                         :element-type '(unsigned-byte 8)
                         :direction :input)
        (let ((class-layouts (make-hash-table))
              (created-objects 0)
              (reported-objects-count 0)
              (read-slots 0)
              (error t)
              (*slot-name-map* nil))
          (unwind-protect
               (progn
                 (with-simple-restart
                     (finalize-object-subsystem "Finalize the object subsystem.")
                   (loop
                      (when (and (plusp created-objects)
                                 (zerop (mod created-objects 10000))
                                 (not (= created-objects reported-objects-count)))
                        #+nil (format t "Snapshot position ~A~%" (file-position s))
                        (report-progress "~A objects created.~%" created-objects)
                        (setf reported-objects-count created-objects)
                        (force-output))
                      (when (and (plusp read-slots)
                                 (zerop (mod read-slots 10000)))
                        (report-progress "~A of ~A objects initialized.~%" read-slots created-objects)
                        (force-output))
                      (let ((char (%read-tag s nil nil)))
                        (unless (member char '(#\I #\L #\O #\S nil))
                          (error "unknown char ~A at offset ~A~%" char (file-position s)))
                        (ecase char
                          ((nil) (return))
                          (#\I (setf (next-object-id (store-object-subsystem)) (%decode-integer s)))
                          (#\L (snapshot-read-layout s class-layouts))
                          (#\O (snapshot-read-object s class-layouts) (incf created-objects))
                          (#\S (snapshot-read-slots s class-layouts) (incf read-slots))))))
                 (map-store-objects #'initialize-transient-slots)
                 (map-store-objects #'initialize-transient-instance)
                 (setf error nil))
            (when error
              (maphash #'(lambda (key val)
                           (declare (ignore key))
                           (let ((class-name (car val)))
                             (report-progress "clearing indices for class ~A~%" (class-name class-name))
                             (clear-class-indices class-name)))
                       class-layouts))))))))

(defun tx-delete-object (id)
  (destroy-object (store-object-with-id id)))

(defun delete-object (object)
  (if (and (in-transaction-p)
           (not (in-anonymous-transaction-p)))
      (destroy-object object)
      (execute (make-instance 'transaction :function-symbol 'tx-delete-object
                              :timestamp (get-universal-time)
                              :args (list (store-object-id object))))))

(defun tx-delete-objects (&rest object-ids)
  (mapc #'(lambda (id) (destroy-object (store-object-with-id id))) object-ids))

(defun delete-objects (&rest objects)
  (if (in-transaction-p)
      (mapc #'destroy-object objects)
      (execute (make-instance 'transaction :function-symbol 'tx-delete-objects
                              :timestamp (get-universal-time)
                              :args (mapcar #'store-object-id objects)))))

(defgeneric cascade-delete-p (object referencing-object)
  (:method (object referencing-object)
    (declare (ignore object referencing-object))
    nil)
  (:documentation "return non-nil if the REFERENCING-OBJECT should be deleted when the OBJECT is deleted"))

(defun partition-list (list predicate)
  "Return two list values, the first containing all elements from LIST
that satisfy PREDICATE, the second those that don't"
  (let (do dont)
    (dolist (element list)
      (if (funcall predicate element)
          (push element do)
          (push element dont)))
    (values do dont)))

(defun cascading-delete-object (object)
  "Delete the OBJECT and all objects that reference it and that are eligible to cascading deletes, as indicated by
the result of calling CASCADE-DELETE-P.  Generate error if there are references to the objects that are not eligible
to cascading deletes."
  (multiple-value-bind (cascading-delete-refs
                        remaining-refs)
      (partition-list (find-refs object) (alexandria:curry #'cascade-delete-p object))
    (when remaining-refs
      (error "Cannot delete object ~A because there are references ~
              to this object in the system, please consult a system administrator!"
             object))
    (apply #'delete-objects object cascading-delete-refs)))

(defun tx-change-slot-values (object &rest slots-and-values)
  "Called by the MOP to change a persistent slot's value."
  (unless (in-transaction-p)
    (error 'not-in-transaction))
  (when object
    (loop for (slot value) on slots-and-values by #'cddr
       do (setf (slot-value object slot) value))))

(defun change-slot-values (object &rest slots-and-values)
  "This function is the deprecated way to set slots of persistent
   objects."
  (warn "CHANGE-SLOT-VALUES is deprecated - use WITH-TRANSACTION and standard accessors!")
  (execute (make-instance 'transaction
                          :function-symbol 'tx-change-slot-values
                          :timestamp (get-universal-time)
                          :args (list* object slots-and-values))))

(defgeneric prepare-for-snapshot (object)
  (:method ((object store-object))
    nil)
  (:documentation "Called for every store object before a snapshot is
  written."))

(defun find-store-object (id-or-name &key (class 'store-object) query-function key-slot-name)
  "Mock up implementation of find-store-object API as in the old datastore.
Note: QUERY-FUNCTION will only be used if ID-OR-NAME is neither an integer nor a
string designating an integer."
  (unless id-or-name
    (error "can't search a store object with null key"))
  (when (stringp id-or-name)
    (multiple-value-bind (value end) (parse-integer id-or-name :junk-allowed t)
      (when (and value
                 (eql end (length id-or-name)))
        (setq id-or-name value))))
  (let ((result (cond
                  ((numberp id-or-name)
                   (store-object-with-id id-or-name))
                  (t
                   (cond
                     (query-function
                      (funcall query-function id-or-name))
                     ((eq class 't)
                      (error "can't search for store object by name without class specified"))
                     (t
                      (let ((index (bknr.indices::class-slot-index (find-class class) key-slot-name)))
                        (when index
                          (index-get index id-or-name)))))))))
    (unless (or (null result)
                (typep result class))
      (error "Object ~A is not of wanted type ~A." result class))
    result))

(deftransaction store-object-add-keywords (object slot keywords)
  (setf (slot-value object slot)
        (union (slot-value object slot)
               keywords)))

(deftransaction store-object-remove-keywords (object slot keywords)
  (setf (slot-value object slot)
        (set-difference (slot-value object slot) keywords)))

(deftransaction store-object-set-keywords (object slot keywords)
  (setf (slot-value object slot) keywords))

(defmethod find-refs ((object store-object))
  "Find references to the given OBJECT in all store-objects, traversing both single valued and list valued slots."
  (remove-if-not
   (lambda (candidate)
     (find-if (lambda (slotd)
                (and (slot-boundp candidate (slot-definition-name slotd))
                     (let ((slot-value (slot-value candidate (slot-definition-name slotd))))
                       (or (eq object slot-value)
                           (and (alexandria:proper-list-p slot-value)
                                (find object slot-value))))))
              (class-slots (class-of candidate))))
   (class-instances 'store-object)))

(pushnew :mop-store cl:*features*)

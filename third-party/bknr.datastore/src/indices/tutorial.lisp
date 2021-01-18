;;; Slot indexes for Common Lisp

;;;# Introduction
;;;
;;; In the framework we built as a backend for the `eboy.com' website,
;;; we built a prevalence layer that could handle CLOS objects. These
;;; CLOS objects all had an `ID', and could be indexed over other
;;; slots as well. For example, we heavily used "keyword indices" that
;;; could give back all objects that had a certain keyword stored in a
;;; slot. However, the slot indices were built into a very big
;;; `define-persistent-class' macro, and could not easily be extended
;;; or used on their own.
;;;
;;; This index layer is now built using the metaobject protocol, and
;;; has a CLOS method protocol to access indices, so that new index
;;; classes can easily be added.
;;;
;;; This tutorial will show you how to create CLOS classes with slot
;;; indices, class indices, and how to create custom indices and use
;;; them with your classes.


;;;# Obtaining and loading BKNR slot indices
;;;
;;; You can obtain the current CVS sources of BKNR by following the
;;; instructions at `http://bknr.net/'. Add the `src' directory of
;;; BKNR to your `asdf:*central-registry*', and load the indices
;;; module by evaluating the following form:

(asdf:oos 'asdf:load-op :bknr.indices)

;;; Then switch to the `bknr.indices' package to try out the tutorial.

(in-package :bknr.indices)

;;;# A simple indexed class
;;;
;;;## A standard non-indexed class
;;;
;;; We begin by defining a simple class called GORILLA. Gorillas have
;;; a name, and a description keyword.

(defclass gorilla ()
  ((name        :initarg :name
		:reader gorilla-name
		:type string)
   (description :initarg :description
		:reader gorilla-description)))

(defmethod print-object ((gorilla gorilla) stream)
  (print-unreadable-object (gorilla stream :type t)
    (format stream "~S" (gorilla-name gorilla))))
  

;;; We can create a few gorillas to test the class. To refer to these
;;; gorillas later on, we have to store them in a list. We can then
;;; write functions to search for gorillas.

(defvar *gorillas* nil)

(setf *gorillas*
      (list
       (make-instance 'gorilla :name "Lucy"
		      :description :aggressive)
       (make-instance 'gorilla :name "Robert"
		      :description :playful)
       (make-instance 'gorilla :name "John"
		      :description :aggressive)))

(defun all-gorillas ()
  (copy-list *gorillas*))

(defun gorilla-with-name (name)
  (find name *gorillas* :test #'string-equal
	:key #'gorilla-name))

(defun gorillas-with-description (description)
  (remove description *gorillas* :test-not #'eql :key
	  #'gorilla-description))

(all-gorillas)
; => (#<GORILLA "Lucy"> #<GORILLA "Robert"> #<GORILLA "John">)
(gorilla-with-name "Lucy")
; => #<GORILLA "Lucy">
(gorillas-with-description :aggressive)
; => (#<GORILLA "Lucy"> #<GORILLA "John">)
(gorilla-with-name "Manuel")
; => NIL

;;; What we would like to do however, is have the object system index
;;; these objects for us. This is achieved by using INDEXED-CLASS as
;;; the metaclass for the gorilla class. The `INDEXED-CLASS' has its
;;; own slot-definition objects called `INDEX-DIRECT-SLOT-DEFINITION'
;;; and `INDEX-EFFECTIVE-SLOT-DEFINITION'. Using these classes, we can
;;; specify additional initargs to our slot definitions.
;;;
;;;## Additional slot initargs
;;;
;;; The following additional initargs are available:
;;;
;;; `INDEX' - A class name that specifies the class of the index to
;;; use. For example `UNIQUE-INDEX', `HASH-INDEX' or
;;; `HASH-LIST-INDEX'.
;;;
;;; `INDEX-INITARGS' - Additional arguments that are passed to
;;; `INDEX-CREATE' when creating the index.
;;;
;;; `INDEX-READER' - A symbol under which a query function for the
;;; index will be stored.
;;;
;;; `INDEX-KEYS' - A symbol under which a function returning all the
;;; values of the index will be stored.
;;;
;;; `INDEX-SUBCLASSES' - Determines if instances of subclasses of this
;;; class will be indexed in the slot index also. Defaults to `T'.
;;;
;;;## A simple indexed class
;;;
;;; Using the `INDEXED-CLASS', we can redefine our gorilla example.

;;; Before we are able to refine GORILLA with a new metaclass, we need
;;; to delete the old class definition:

(setf (find-class 'gorilla) nil)

(defclass gorilla ()
  ((name :initarg :name :reader gorilla-name
	 :index-type unique-index
	 :index-initargs (:test #'equal)
	 :index-reader gorilla-with-name
	 :index-values all-gorillas)
   (description :initarg :description
		:reader gorilla-description
		:index-type hash-index
		:index-reader gorillas-with-description))
  (:metaclass indexed-class))

(defmethod print-object ((gorilla gorilla) stream)
  (print-unreadable-object (gorilla stream :type t)
    (format stream "~S" (gorilla-name gorilla))))

;;; We have to recreate the gorillas though, as the old instances
;;; don't get updated for now.

(make-instance 'gorilla :name "Lucy" :description :aggressive)
(make-instance 'gorilla :name "Robert" :description :playful)
(make-instance 'gorilla :name "John" :description :aggressive)

(all-gorillas)
; => (#<GORILLA "Lucy"> #<GORILLA "Robert"> #<GORILLA "John">)
(gorilla-with-name "Lucy")
; => #<GORILLA "Lucy">
; T
(gorillas-with-description :aggressive)
; => (#<GORILLA "John"> #<GORILLA "Lucy">)
; T

;;;## Class indices
;;;
;;; We can also create indices that are not bound to a single
;;; slot. These indices are called `CLASS-INDICES'. For example, we
;;; can add two slots for the coordinates of the gorilla, and a class
;;; index of type `ARRAY-INDEX' that will index the two slots `X' and
;;; `Y' of the gorilla in an array of dimensions `256x256'. Note that
;;; redefining the class conserves the existing indices.

(defclass gorilla ()
  ((name :initarg :name :reader gorilla-name
	 :index-type unique-index
	 :index-initargs (:test #'equal)
	 :index-reader gorilla-with-name
	 :index-values all-gorillas)
   (description :initarg :description
		:reader gorilla-description
		:index-type hash-index
		:index-reader gorillas-with-description)
   (x :initarg :x :reader gorilla-x)
   (y :initarg :y :reader gorilla-y))
  (:metaclass indexed-class)
  (:class-indices (coords :index-type array-index
			  :slots (x y)
			  :index-reader gorilla-with-coords
			  :index-initargs (:dimensions '(256 256)))))

(make-instance 'gorilla :name "Pete" :description
	       :playful :x 5 :y 8)

(gorilla-with-coords '(5 8))
; => #<GORILLA "Pete">
(all-gorillas)
; => (#<GORILLA "Lucy"> #<GORILLA "Robert">
;     #<GORILLA "John"> #<GORILLA "Pete">)
(gorillas-with-description :playful)
; => (#<GORILLA "Pete"> #<GORILLA "Robert">)
; T

(let ((lucy (gorilla-with-name "Lucy")))
  (with-slots (x y) lucy
    (setf x 0 y 0)))

(gorilla-with-name "Lucy")
; => #<GORILLA "Lucy">
; T
(gorilla-with-coords '(0 0))
; => #<GORILLA "Lucy">

;;;# Creating indexed classes
;;;
;;; Adding indexes to a class is very simple. The class has to have
;;; the metaclass `INDEXED-CLASS', or a class deriving from
;;; `INDEXED-CLASS'.
;;;

;;;## Slot indices
;;;
;;; `INDEXED-CLASS' uses its own `EFFECTIVE-SLOT-DEFINITION' and
;;; `DIRECT-SLOT-DEFINITION' which add indices to slots. A slot
;;; definition in the `DEFCLASS' form supports additional keyword
;;; arguments:
;;;
;;; `:INDEX' - Specifies an existing index to use as slot-index for this slot.
;;; 
;;; `:INDEX-TYPE' - Specifies the class of the index to be used for this
;;; slot.
;;;
;;; `:INDEX-INITARGS' - Specifies additional initargs to be given to
;;; `INDEX-CREATE' when creating the index. The slot-name is given as
;;; the `:SLOT' keyword argument to `INDEX-CREATE'.
;;;
;;; `:INDEX-READER' - Specifies the name under which a query function
;;; for the created index will be saved.
;;;
;;; `:INDEX-VALUES' - Specifies the name under which a function returning
;;; all the objects stored in the created index will be saved.
;;;
;;; `:INDEX-MAPVALUES' - Specifies the name under which a function
;;; applying a function to all the objects stored in the created index
;;; will be saved.
;;;
;;; `:INDEX-SUBCLASSES' - Specifies if subclasses of the class will
;;; also be indexing in this index. Default is `T'.
;;;
;;; For each `DIRECT-SLOT-DEFINITION' of an indexed class with the
;;; `:INDEX' keyword, an index is created and stored in the
;;; `DIRECT-SLOT-DEFINITION'. All the direct indexes are then stored
;;; in the `EFFECTIVE-SLOT-DEFINITION' (indexes with
;;; `INDEX-SUBCLASSES = NIL' will not).
;;;
;;; Every access to the slot will update the indices stored in the
;;; `EFFECTIVE-SLOT-DEFINITION'. When the slot is changed, the object
;;; is removed from all the slot indices, and added after the slot
;;; value has been changed. When a slot is made unbound, the object is
;;; removed from the slot indices.

(defclass test-slot ()
  ((a :initarg :a :index-type unique-index
      :reader test-slot-a
      :index-reader test-slot-with-a
      :index-values all-test-slots)
   (b :initarg :b :index-type unique-index
      :index-reader test-slot-with-b
      :index-subclasses nil
      :index-values all-test-slots-bs))
  (:metaclass indexed-class))

(defclass test-slot2 (test-slot)
  ((b :initarg :b :index-type unique-index
      :index-reader test-slot2-with-b
      :index-subclasses nil
      :index-mapvalues map-test-slot2s
      :index-values all-test-slot2s-bs))
  (:metaclass indexed-class))

(defmethod print-object ((object test-slot) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (test-slot-a object))))

(make-instance 'test-slot :a 1 :b 2)
(make-instance 'test-slot :a 2 :b 3)
(make-instance 'test-slot2 :a 3 :b 4)
(make-instance 'test-slot2 :a 4 :b 2)
(make-instance 'test-slot2 :a 5 :b 9)

(all-test-slots)
; => (#<TEST-SLOT 1> #<TEST-SLOT 2> #<TEST-SLOT2 3>
;     #<TEST-SLOT2 4> #<TEST-SLOT2 5>)
(test-slot-with-a 2)
; => #<TEST-SLOT 2>
(all-test-slots-bs)
; => (#<TEST-SLOT 1> #<TEST-SLOT 2>)
(all-test-slot2s-bs)
; (#<TEST-SLOT2 3> #<TEST-SLOT2 4> #<TEST-SLOT2 5>)
(map-test-slot2s (lambda (obj) (print obj)))
; 
; #<TEST-SLOT2 3> 
; #<TEST-SLOT2 4> 
; #<TEST-SLOT2 5> 
; 
; NIL

;;; Here is an example of a slot index using an already existing index.

(defvar *existing-unique-index*
  (index-create 'unique-index :slots '(a)))

(defclass test-slot3 ()
  ((a :initarg :a :index *existing-unique-index*))
  (:metaclass indexed-class))

(make-instance 'test-slot3 :a 3)
(make-instance 'test-slot3 :a 4)

(index-get *existing-unique-index* 4)
; => #<TEST-SLOT3 {493B9655}>
; T
(index-values *existing-unique-index*)
; => (#<TEST-SLOT3 {493A0CBD}> #<TEST-SLOT3 {493B9655}>)

;;; The slot indices of a class can be examined using
;;; `CLASS-SLOT-INDICES'.

(class-slot-indices (find-class 'test-slot) 'a)
; => (#<UNIQUE-INDEX SLOT: A SIZE: 5 {599FA9F5}>)
(class-slot-indices (find-class 'test-slot) 'b)
; => (#<UNIQUE-INDEX SLOT: B SIZE: 2 {59A038BD}>)
(class-slot-indices (find-class 'test-slot2) 'a)
; => (#<UNIQUE-INDEX SLOT: A SIZE: 5 {599FA9F5}>)
(class-slot-indices (find-class 'test-slot2) 'b)
; => (#<UNIQUE-INDEX SLOT: B SIZE: 3 {59A0D6A5}>)

;;; Note that a slot can have multiple indices.

(defclass test-slot4 (test-slot)
  ((a :initarg :a :index-type unique-index
      :index-reader test-slot4-with-a
      :index-values all-test-slot4s))
  (:metaclass indexed-class))

(make-instance 'test-slot4 :a 6 :b 9)

(all-test-slots)
; => (#<TEST-SLOT 1> #<TEST-SLOT 2> #<TEST-SLOT2 3>
;     #<TEST-SLOT2 4> #<TEST-SLOT2 5>
;     #<TEST-SLOT4 6>)
(all-test-slot4s)
; => (#<TEST-SLOT4 6>)
(class-slot-indices (find-class 'test-slot4) 'a)
; => (#<UNIQUE-INDEX SLOT: A SIZE: 6 {599FA9F5}>
;  #<UNIQUE-INDEX SLOT: A SIZE: 1 {59079E25}>)


;;;## Class indices
;;;
;;; In addition to slot indices, an indexed class supports class
;;; indices which react when one of several slots is changing. For
;;; example, in the `GORILLA' class above, the `COORDS' index reacts
;;; on slots `X' and `Y'. By default, a class index reacts on all
;;; slots.
;;;
;;; A class index is created by adding a class option `CLASS-INDICES'
;;; followed by a list of class index specifications.

(defclass test-class ()
  ((x :initarg :x :reader test-class-x)
   (y :initarg :y :reader test-class-y)
   (z :initarg :z :reader test-class-z))
  (:metaclass indexed-class)
  (:class-indices (2d-coords :index-type array-index :slots (x y)
			     :index-initargs (:dimensions '(256 256))
			     :index-reader test-with-2d-coords)
		  (3d-coords :index-type array-index :slots (x y z)
			     :index-reader test-with-3d-coords
			     :index-initargs (:dimensions '(256 256 2)))))

(defmethod print-object ((object test-class) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y z) object
      (format stream "~d,~d,~d" x y z))))

(make-instance 'test-class :x 1 :y 1 :z 0)
(make-instance 'test-class :x 1 :y 3 :z 1)
(make-instance 'test-class :x 1 :y 2 :z 0)

(test-with-3d-coords '(1 1 0))
; => #<TEST-CLASS 1,1,0>
(test-with-2d-coords '(1 1))
; => #<TEST-CLASS 1,1,0>
(test-with-2d-coords '(1 2))
; => #<TEST-CLASS 1,2,0>

;;; A class index specification has to comply with the following
;;; lambda-list `(NAME &REST ARGS &KEY INDEX-READER INDEX-VALUES SLOTS
;;; TYPE INDEX &ALLOW-OTHER-KEYS)'. The key arguments `:INDEX-TYPE',
;;; `:INDEX', `:INDEX-READER' and `:INDEX-VALUES' are then removed from
;;; the initargs, and the rest is passed to `INDEX-CREATE' to create
;;; the class index.
;;;
;;; `:INDEX-TYPE' - specifies the type of the class index.
;;;
;;; `:INDEX' - (optional) specifies an already existing index object
;;; to use.
;;;
;;; `:INDEX-READER' - Like `:INDEX-READER' for slot
;;; indices.
;;;
;;; `:INDEX-VALUES' - Like `:INDEX-VALUES' for slot indices.
;;;
;;; Using `:INDEX', we can use already existing indices as class
;;; indices.

(defvar *array-index*
  (index-create 'array-index :slots '(x y z)
		:dimensions '(256 256 2)))

(defclass test-class2 (test-class)
  ()
  (:metaclass indexed-class)
  (:class-indices (coords :index *array-index* :slots (x y z)
			  :index-reader test-with-coords)))

(make-instance 'test-class2 :x 5 :y 5 :z 0)

*array-index*
; => #<ARRAY-INDEX SLOTS: (X Y Z) ((256 256 2)) {593F383D}>
(index-get *array-index* '( 5 5 0))
; => #<TEST-CLASS2 5,5,0>
(test-with-coords '(5 5 0))
; => #<TEST-CLASS2 5,5,0>

;;; XXX the class index tutorial needs updating, please skip to next section!

;;; Another example of a class index is the `CLASS-INDEX' index.

(defvar *class-index* (index-create 'class-index))

(defclass base-object ()
  ()
  (:metaclass indexed-class)
  (:class-indices (class :index *class-index*
			 :slots nil
			 :index-reader objects-of-class
			 :index-values all-objects
			 :index-subclasses t
			 :index-keys all-class-names)
		  (classes :index-type class-index
			   :index-initargs (:index-superclasses t)
			   :slots nil
			   :index-subclasses t
			   :index-reader objects-with-class)))

(defclass child1 (base-object)
  ()
  (:metaclass indexed-class))

(defclass child2 (base-object)
  ((a :initarg :a))
  (:metaclass indexed-class))

(make-instance 'child1)
(make-instance 'child1)
(make-instance 'child1)
(make-instance 'child2)
(make-instance 'child2)

(all-objects)
; => (#<CHILD1 {48E5CB3D}> #<CHILD1 {48E51395}> #<CHILD1 {48E453DD}>
;  #<CHILD2 {48E82F55}> #<CHILD2 {48E7746D}>)
(objects-with-class 'child1)
; => (#<CHILD1 {48E5CB3D}> #<CHILD1 {48E51395}> #<CHILD1 {48E453DD}>)
; T
(objects-with-class 'child2)
; => (#<CHILD2 {48E82F55}> #<CHILD2 {48E7746D}>)
; T
(objects-with-class 'base-object)
; => (#<CHILD2 {48E82F55}> #<CHILD2 {48E7746D}> #<CHILD1 {48E5CB3D}>
;  #<CHILD1 {48E51395}> #<CHILD1 {48E453DD}>)
; T
(objects-of-class 'child1)
; => (#<CHILD1 {48E5CB3D}> #<CHILD1 {48E51395}> #<CHILD1 {48E453DD}>)
; T
(objects-of-class 'child2)
; => (#<CHILD2 {48E82F55}> #<CHILD2 {48E7746D}>)
; T
(objects-of-class 'base-object)
; => NIL
; NIL

;;;## Destroying objects
;;;
;;; Indexed objects will not be garbage collected until they are
;;; removed from the indices. This is done by calling the
;;; `DESTROY-OBJECT' method on the object. This removes the object
;;; from all its indices, and sets the slot `DESTROYED-P' to `T', so
;;; that not slot-access is possible anymore on the object.

(make-instance 'test-class2 :x 5 :y 5 :z 0)

(let ((obj (test-with-coords '(5 5 0))))
  (destroy-object obj)

;;; This will throw an error:
;;;   Can not get slot X of destroyed object of class TEST-CLASS.

  (test-class-x obj))

;;;## Class and object reinitialization
;;;
;;; When a class is redefined, the indexed-class code tries to map
;;; the new slot-indices to the old-indices. If it finds a slot-index
;;; in the old `EFFECTIVE-SLOT-DEFINITION' and a slot-index in the new
;;; `EFFECTIVE-SLOT-DEFINITION', it calls `INDEX-REINITIALIZE' on the
;;; two indices to copy the values form the old index to the new
;;; one. Afterwards, the same is done for the class
;;; indices. `INDEX-REINITIALIZE' will not be called with the
;;; old-index being the same as the new-index, so that explicitly
;;; instantiated class indices don't get reinitialized with
;;; themselves.
;;;
;;; Indices for new slots or new class indices are obviously empty on
;;; creation, and will be filled when the existing instances are
;;; updated. For now, `SHARED-INITIALIZE' is not overloaded, so the
;;; instance updates are noticed through `(SETF SLOT-VALUE-USING-CLASS)'.

;;;# Creating a custom index
;;;
;;; The main reason to write indexed slots was to be able to use
;;; custom indices that are appropriate for the task at hand. Indices
;;; are CLOS objects that follow the index method protocol. The
;;; methods that have to be implemented are:
;;;
;;; `INDEX-ADD (INDEX OBJECT)' - Add OBJECT to the INDEX. Throws an
;;; ERROR if a problem happened while inserting OBJECT."
;;;
;;; `INDEX-GET (INDEX KEY)' - Get the object (or the objects) stored
;;; under the index-key KEY.
;;;
;;; `INDEX-REMOVE (INDEX OBJECT)' - Remove OBJECT from the INDEX.
;;;
;;; `INDEX-KEYS (INDEX)' - Returns all the keys of the index.
;;;
;;; `INDEX-VALUES (INDEX)' - Returns all the objects stored in INDEX.
;;;
;;; `INDEX-REINITIALIZE (NEW-INDEX OLD-INDEX)' - Called when the
;;; definition of an index is changed.
;;;
;;; `INDEX-CLEAR (INDEX)' - Remove all indexed objects from the index.
;;;
;;; In addition to these methods, there is the function `INDEX-CREATE'
;;; that instantiates an index object.  It passes the `INDEX-INITARGS'
;;; provided to the `MAKE-INSTANCE' call that creates the index.
;;;
;;; The best way to see how this methods are used is to have at look
;;; at the basic index `SLOT-INDEX'. A unique index indexes an object
;;; under a key stored in a slot of this object, so a slot index is
;;; initialized using two arguments: the slot-name where the key is
;;; stored, and a test to create the underlying hash-table.

(defclass slot-index ()
  ((hash-table :initarg :hash-table :accessor slot-index-hash-table
	       :documentation "The internal hash table used to index
objects.")
   (slot-name :initarg :slot-name :reader slot-index-slot-name
	      :documentation "The value of the slot with name
SLOT-NAME is used as a key to the internal hash-table.")
   (index-nil :initarg :index-nil :reader slot-index-index-nil
	      :initform nil
	      :documentation "If T, NIL is used as a valid slot
 value, else slots with NIL value are treated as unbound slots.")))

(defmethod initialize-instance :after ((index slot-index) &key (test #'eql) slots index-nil)
  (unless (<= (length slots) 1)
    (error "Can not create slot-index with more than one slot."))
  (with-slots (hash-table slot-name) index
    (setf hash-table (make-hash-table :test test)
	  slot-name (first slots)
	  (slot-value index 'index-nil) index-nil)))

;;; When a class is redefined, the indices are re-created. However, we
;;; still want our existing objects to be indexed by the new index,
;;; therefore `INDEX-REINITIALIZE' copies the hash-table when the
;;; hash-table test is the same, or else copies all the stored objects
;;; into the new hash-table.

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
	(loop for key being the hash-keys of old-hash
	      using (hash-value value)
	      do (setf (gethash key new-hash) value)))
    new-index))

;;; `INDEX-CLEAR' just creates an empty hash-table to replace the
;;; existing hash-table.

(defmethod index-clear ((index slot-index))
  (with-slots (hash-table) index
    (setf hash-table (make-hash-table
		      :test (hash-table-test hash-table)))))

;;; `INDEX-ADD' and `INDEX-REMOVE' both use the slot-name to get the
;;; key value, and use this key to query the underlying
;;; hash-table. `INDEX-ADD' is not defined for the base class
;;; `SLOT-INDEX', however it is defined in the simple child class
;;; `UNIQUE-INDEX'. When another object is stored under the key, an
;;; error is thrown.

(defclass unique-index (slot-index)
  ())

(defmethod index-add ((index unique-index) object)
  "Add an object using the value of the specified slot as key.
When the hash-table entry already contains a value, an error
is thrown."
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

(defmethod index-remove ((index slot-index) object)
  (let ((slot-name (slot-index-slot-name index)))
    (if (slot-boundp object slot-name)
	(remhash (slot-value object slot-name)
		 (slot-index-hash-table index))
	(warn "Ignoring request to remove object ~a
with unbound slot ~A."
	      object slot-name))))

;;; The rest of the methods are straightforward.

(defmethod index-get ((index slot-index) key)
  (gethash key (slot-index-hash-table index)))

(defmethod index-keys ((index slot-index))
  (loop for key being the hash-keys
	of (slot-index-hash-table index)
	collect key))

(defmethod index-values ((index slot-index))
  (loop for value being the hash-values
	of (slot-index-hash-table index)
	collect value))

;;;# Creating an index using multiple slots
;;;
;;; When creating an index using multiple slots, you have to take care
;;; of a few things. It can happen that a slot-value used by the index
;;; is updated, but that the other slots that are needed are
;;; unbound. However, this is not always an error, so a class index
;;; has to check that all the slots it needs are bound. This is the
;;; `INDEX-ADD' method for an array index.

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
	  do (error "Could not add ~a to array-index ~a
because the coordinates ~a are out of bound."
		    object index slot-values))
    (let ((value (apply #'aref array slot-values)))
      (when (and value
		 (not (eql value object)))
	(error (make-condition 'index-existing-error
			       :index index :key slot-values
			       :value value))))
    (setf (apply #'aref array slot-values)
	  object)))


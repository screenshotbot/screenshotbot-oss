;;; BKNR Datastore

;;;# Introduction
;;;
;;;## The prevalence model
;;;
;;; The BKNR datastore is a persistence solution for Lisp data. It
;;; uses the prevalence model, which is based on the following
;;; assumptions:
;;;
;;; All data is held in RAM.
;;;
;;; Data can be saved to disk at once into a snapshot file and is read
;;; from that file at startup time.
;;;
;;; Changes to persistent data are written to a transaction log file
;;; immediately, which can be replayed to restore all changes that
;;; occured since the last snapshot was saved.
;;;
;;; Every kind of operation that needs to be logged is called a
;;; "transaction", and such transactions are made explicit in the
;;; program code. This is different from object-oriented databases,
;;; where the fundamental transactions are object creation, object
;;; deletion and slot access, which are not special cases in the
;;; prevalence model at all.
;;;
;;; Isolation of transactions is achieved using thread locks. In the
;;; simplest model used by the `mp-store', transactions are serialized
;;; using a global lock.
;;;
;;; The transaction system is responsible for providing replay of
;;; committed transactions after a server crash, but not for rollback
;;; of failed transactions in a running server, except that failing
;;; transactions are simply not logged onto disk. To roll back
;;; transactions at points where exceptions might be excepted, use
;;; ordinary Lisp programming techniques involving `unwind-protect'
;;; and similar.
;;;
;;;## BKNR Datastore Design
;;;
;;; The design of the datastore aims to make explicit the
;;; orthogonality of object system access (unlogged) and logging of
;;; transactions (essentially independent of the object system). The
;;; interface between transaction system and object system is
;;; documented and allows for the implementation of alternative object
;;; systems. For example, the blob subsystem is using the same
;;; interface as the object subsystem.
;;;
;;; Previous versions of the BKNR Datastore allowed the creation of
;;; multiple datastores in a single LISP process. However, this
;;; feature was seldom used, and could be very confusing while
;;; developing applications. The new version of the BKNR Datastore
;;; supports only a single datastore, which is referenced by the
;;; special variable `*STORE*'.
;;;
;;;## BKNR Object Datastore
;;;
;;; In addition to the transaction layer (in the file `txn.lisp'), the
;;; BKNR datastore provides persistent CLOS objects using the
;;; Metaobject Protocol. It provides a metaclass with which slots can
;;; be defined as persistent (stored on snapshot) or transient. The
;;; metaclass also prohibits slot accesses outside transactions,
;;; provides unique IDs for all objects, and provides standard query
;;; functions like `STORE-OBJECTS-WITH-CLASS' and
;;; `STORE-OBJECTS-OF-CLASS'. The object datastore can be seamlessly
;;; combined with BKNR indices and XML import/export.

;;;# Obtaining and loading BKNR Datastore
;;;
;;; You can obtain the current CVS sources of BKNR by following the
;;; instructions at `http://bknr.net/blog/bknr-devel'. Add the `experimental'
;;; directory of BKNR to your `asdf:*central-registry*', and load the
;;; indices module by evaluating the following form:

(asdf:oos 'asdf:load-op :bknr.datastore)

;;; Then switch to the `bknr.datastore' package to try out the tutorial.

(in-package :bknr.datastore)

;;;# A transaction system example

;;; The first datastore we will build is very simple. We have a
;;; counter variable for the store, and this counter variable can be
;;; decremented and indecremented. We want this variable to be
;;; persistent, so decrementing and incrementing it has to be done
;;; through transactions that will be logged by the datastore. We also
;;; define a `:BEFORE' method for the generic function `RESTORE-STORE'
;;; to set the counter to `0' initially. This method will be called
;;; every time the store is created or restored from disk.

(defclass tutorial-store (mp-store)
  ((counter :initform 0 :accessor tutorial-store-counter)))

(defmethod restore-store :before ((store tutorial-store) &key until)
  (declare (ignore until))
  (setf (tutorial-store-counter store) 0))

;;; The two transactions are declared like normal functions, but using
;;; the `DEFTRANSACTION' macro. 

(deftransaction incf-counter ()
  (incf (tutorial-store-counter *store*)))

(deftransaction decf-counter ()
  (decf (tutorial-store-counter *store*)))

;;; When looking at the macro-expanded form of `DEFTRANSACTION', we
;;; see that `DEFTRANSACTION' defines two functions, a toplevel
;;; function that creates a transaction object and calls the method
;;; `EXECUTE' on it, and a function that contains the actual
;;; transaction code and that will be called in the context of the
;;; transaction, and logged to disk.

(PROGN (DEFUN TX-DECF-COUNTER ()
         (UNLESS (IN-TRANSACTION-P) (ERROR 'NOT-IN-TRANSACTION))
         (DECF (TUTORIAL-STORE-COUNTER *STORE*)))
       (DEFUN DECF-COUNTER ()
         (EXECUTE (MAKE-INSTANCE 'TRANSACTION
                                 :FUNCTION-SYMBOL 'TX-DECF-COUNTER
                                 :TIMESTAMP (GET-UNIVERSAL-TIME)
                                 :ARGS (LIST)))))

;;; The new datastore only supports a single datastore instance per
;;; LISP instance. When creating a `STORE' object, the `*STORE*'
;;; special variable is modified to point to the datastore. Thus, we
;;; can create our simple datastore by creating an object of type
;;; `TUTORIAL-STORE'. The transaction log will be store in the
;;; directory "/tmp/tutorial-store".

(close-store)
(make-instance 'tutorial-store :directory "/tmp/tutorial-store/"
                               :subsystems nil)
; Warning:  restoring #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
; => #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">

(tutorial-store-counter *store*)
; => 0
(incf-counter)
; => 1
(incf-counter)
; => 2
(decf-counter)
; => 1

;;; The three transactions have been logged to the transaction log in
;;; "/tmp/tutorial-store/", as we can see:

(with-open-file (s "/tmp/tutorial-store/current/transaction-log"
                   :direction :input)
  (file-length s))
; => 126
(incf-counter)
; => 2
(with-open-file (s "/tmp/tutorial-store/current/transaction-log"
                   :direction :input)
  (file-length s))
; => 168

;;; The transaction log is kept in a directory called "current", which
;;; is where the currently active version of the snapshots and log
;;; files are kept. When a datastore is snapshotted, the "current"
;;; directory is backupped to another directory with the current date,
;;; and snapshots are created in the new "current" directory. However,
;;; we cannot snapshot our tutorial datastore, as we cannot snapshot
;;; the persistent data (the counter value). 

(snapshot)
; => Error in function (METHOD SNAPSHOT-STORE NIL (STORE)):
; => Cannot snapshot store without subsystems...
; => [Condition of type SIMPLE-ERROR]

;;; We can close the store by using the function `CLOSE-STORE'.
*store*
; => #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
(close-store)
; => NIL
(boundp '*store*)
; => NIL

;;; The store can then be recreated, and the transaction log will be
;;; read and executed upon restore.
(make-instance 'tutorial-store :directory "/tmp/tutorial-store/"
                               :subsystems nil)

; Warning:  restoring #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
; Warning:  loading transaction log
; /tmp/tutorial-store/current/transaction-log
; => #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
(tutorial-store-counter *store*)
; => 2

;;; The store can also be restored in a later LISP session. Make sure
;;; that all the code necessary to the execution of the transaction
;;; log has been loaded before restoring the datastore. A later
;;; version of the datastore will log all the code necessary in the
;;; datastore itself, so that code and data are synchronized.

;;;## Debugging the datastore
;;;
;;; By setting the `*STORE-DEBUG*' special variable to `T', the
;;; datastore prints a lot of useful warnings. For example

;;; You can also restore to a certain point in time, by specifying the
;;; `UNTIL' argument of `RESTORE-STORE'.

(setf *store-debug* t)
; => T
(restore-store *store*)
; restoring #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
; loading transaction log /tmp/tutorial-store/current/transaction-log
; executing transaction #<TRANSACTION 21.04.2008 07:08:22 TX-INCF-COUNTER > at timestamp 3417743302
; executing transaction #<TRANSACTION 21.04.2008 07:08:25 TX-INCF-COUNTER > at timestamp 3417743305
; executing transaction #<TRANSACTION 21.04.2008 07:08:26 TX-DECF-COUNTER > at timestamp 3417743306
; executing transaction #<TRANSACTION 21.04.2008 07:08:34 TX-INCF-COUNTER > at timestamp 3417743314
; => NIL
(tutorial-store-counter *store*)
; => 2
; !! Update the timestamp below to correspond to the fist transaction executed above !!
(restore-store *store* :until 3417743302) ...
; restoring #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
; loading transaction log /tmp/tutorial-store/current/transaction-log
; executing transaction #<TRANSACTION 21.04.2008 07:08:22 TX-INCF-COUNTER > at timestamp 3417743302
; creating log file backup: /tmp/tutorial-store/current/transaction-log.backup
; truncating transaction log at position 42.
(tutorial-store-counter *store*)
; => 1

;;;## Adding a subsystem

;;; Now that we can restore the counter state by loading the
;;; transaction log, we want to add a subsystem to be able to snapshot
;;; the state of the counter. Thus, we won't need to execute every
;;; single incrementing or decrementing transaction to restore our
;;; persistent state.

;;; To do this, we have to create a store-subsystem that will be able
;;; to write the counter number to a file and to reload it on restore.

(defclass counter-subsystem ()
  ())

;;; Three methods are used to interact with the subsystem.
;;; The first method is `INITIALIZE-SUBSYSTEM', which is called after
;;; the store has been created and restored. It is used to initialize
;;; certain parameters of the subsystem. We won't use this method
;;; here, as our subsystem is very simple.
;;; The second method is `SNAPSHOT-SUBSYSTEM', which is called when
;;; the store is snapshotted. The subsystem has to store the
;;; persistent data it handles to a snapshot file inside the current
;;; directory of the store. Our `COUNTER-SUBSYSTEM' writes the current
;;; value of the counter to a file named "counter" in the current
;;; directory of the store (the old directory has been renamed).

(defmethod snapshot-subsystem ((store tutorial-store)
			       (subsystem counter-subsystem))
  (let* ((store-dir (ensure-store-current-directory store))
	 (counter-pathname
	  (make-pathname :name "counter" :defaults store-dir)))
    (with-open-file (s counter-pathname :direction :output)
      (write (tutorial-store-counter store) :stream s))))

;;; Finally, the method `RESTORE-SUBSYSTEM' is called at restore time
;;; to tell the subsystem to read back its persistent state from the
;;; current directory of the store. Our `COUNTER-SUBSYSTEM' reads back
;;; the counter value from the file named "counter". If it can't find
;;; the file (for example if this is the first time that our datastore
;;; is created, the file won't be there, so we issue a warning and set
;;; the counter value to 0.

(defmethod restore-subsystem ((store tutorial-store)
			      (subsystem counter-subsystem) &key
			      until)
  (declare (ignore until))
  (let* ((store-dir (ensure-store-current-directory store))
	 (counter-pathname
	  (make-pathname :name "counter" :defaults store-dir)))
    (if (probe-file counter-pathname)
	(with-open-file (s counter-pathname :direction :input)
	  (let ((counter (read s)))
	    (setf (tutorial-store-counter store) counter)))
	(progn
	  (warn "Could not find store counter value, setting to 0.")
	  (setf (tutorial-store-counter store) 0)))))

;;; Now we can close our current store, and instantiate it anew with a
;;; `COUNTER-SUBSYSTEM'.

(close-store)
; => NIL
(make-instance 'tutorial-store :directory "/tmp/tutorial-store/"
                               :subsystems (list (make-instance 'counter-subsystem)))
; Warning:  restoring #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
; Warning:  Could not find store counter value, setting to 0.
; Warning:  loading transaction log
; /tmp/tutorial-store/current/transaction-log
; => #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
(snapshot)
; Snapshotting subsystem #<COUNTER-SUBSYSTEM #xE65F866> of #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
; Successfully snapshotted #<COUNTER-SUBSYSTEM #xE65F866> of #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
; => NIL
(restore)
; restoring #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
; Restoring the subsystem #<COUNTER-SUBSYSTEM #xE65F866> of #<TUTORIAL-STORE DIR: "/tmp/tutorial-store/">
; => NUL

;;;# An object store example

;;; The BKNR object datastore is implemented using a special subsystem
;;; `STORE-OBJECT-SUBSYSTEM'. Every object referenced by the store
;;; object subsystem has a unique ID, and must be of the class
;;; `STORE-OBJECT'. The ID counter in the store-object subsystem is
;;; incremented on every object creation.
;;;
;;; All store objects have to be of the metaclass `PERSISTENT-CLASS',
;;; which will ensure the object is referenced in the base indices of
;;; the object datastore, and that slot access is only done inside a
;;; transaction. The subsystem makes heavy use of BKNR indices, and
;;; indexes object by ID and by class.

;;; The ID index can be queried using the functions
;;; `STORE-OBJECT-WITH-ID', which returns the object with the
;;; requested ID, `ALL-STORE-OBJECTS' which returns all current store
;;; objects, and `MAP-STORE-OBJECTS', which applies a function
;;; iteratively to each store object. The class index can be queried
;;; using the functions `ALL-STORE-CLASSES', which returns the names
;;; of all the classes currently present in the datastore, and
;;; `STORE-OBJECTS-WITH-CLASS', which returns all the objects of a
;;; specific class (across superclasses also, so
;;; `(STORE-OBJECTS-WITH-CLASS \'STORE-OBJECT)' returns all the
;;; existing store objects.

;;;## Store and object creation

;;; We can create an object datastore by creating a `STORE' with the
;;; subsystem `STORE-OBJECT-SUBSYSTEM'.

(close-store)
(make-instance 'mp-store :directory "/tmp/object-store/"
                         :subsystems (list
                                      (make-instance 'store-object-subsystem)))

; Warning:  restoring #<MP-STORE DIR: "/tmp/object-store/">
; restoring #<MP-STORE DIR: "/tmp/object-store/">
; Restoring the subsystem #<STORE-OBJECT-SUBSYSTEM #xE63F866> of #<MP-STORE DIR: "/tmp/object-store/">
(all-store-objects)
; => NIL

;;; We can now create a few store objects (which is not very
;;; interesting in itself). Store objects have to be created inside a
;;; transaction so that the object creation is logged into the
;;; transaction log.  If `MAKE-INSTANCE' is used outside of a
;;; transaction, the datastore will create the object in a new,
;;; separate transaction.

(make-instance 'store-object)
; => #<STORE-OBJECT ID: 0>
(make-instance 'store-object)
; => #<STORE-OBJECT ID: 1>
(all-store-objects)
; => (#<STORE-OBJECT ID: 0> #<STORE-OBJECT ID: 1>)
(all-store-classes)
; => (STORE-OBJECT)

;;; Object deletion also has to be done through the transaction
;;; `DELETE-OBJECT', which will log the deletion of the object in the
;;; transaction log, and remove the object from all its indices.

(make-instance 'store-object)
; executing transaction #<TRANSACTION 21.04.2008 08:02:10 MAKE-INSTANCE STORE-OBJECT ID 2> at timestamp 3417746530
; => #<STORE-OBJECT ID: 12>
(store-object-with-id 2)
; => #<STORE-OBJECT ID: 2>
(delete-object (store-object-with-id 2))
; executing transaction #<TRANSACTION 21.04.2008 08:52:14 TX-DELETE-OBJECT 2> at timestamp 3417749534
; => T
(store-object-with-id 2)
; => NIL

;;;## Defining persistent classes

;;; A more interesting thing is to create our own persistent class,
;;; which we will call `TUTORIAL-OBJECT'.

(defclass tutorial-object (store-object)
  ((a :initarg :a :reader tutorial-object-a))
  (:metaclass persistent-class))

;;; We can also use the `DEFINE-PERSISTENT-CLASS' to define the class
;;; `TUTORIAL-OBJECT':

(define-persistent-class tutorial-object ()
  ((a :read)))

;;; This gets macroexpanded to the following form.

(DEFINE-BKNR-CLASS TUTORIAL-OBJECT
    (STORE-OBJECT)
  ((A :READ))
  (:METACLASS PERSISTENT-CLASS))

;;; The macro DEFINE-BKNR-CLASS is a short form of the macro DEFCLASS,
;;; it expands to the following code. The `EVAL-WHEN' is
;;; there to ensure timely definition of the accessor methods.

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (DEFCLASS TUTORIAL-OBJECT
      (STORE-OBJECT)
    ((A :READER TUTORIAL-OBJECT-A :INITARG :A))
    (:METACLASS PERSISTENT-CLASS)))

;;; We can now create a few instance of `TUTORIAL-OBJECT':

(make-instance 'tutorial-object :a 2)
; => #<TUTORIAL-OBJECT ID: 3>
(make-instance 'tutorial-object :a 2)
; => #<TUTORIAL-OBJECT ID: 4>
(make-instance 'tutorial-object :a 2)
; => #<TUTORIAL-OBJECT ID: 5>

(store-object-with-id 5)
; => #<TUTORIAL-OBJECT ID: 5>

(all-store-classes)
; => (STORE-OBJECT TUTORIAL-OBJECT)

(store-objects-with-class 'tutorial-object)
; => (#<TUTORIAL-OBJECT ID: 3> #<TUTORIAL-OBJECT ID: 4>
;     #<TUTORIAL-OBJECT ID: 5>)
(store-objects-with-class 'store-object)
; => (#<STORE-OBJECT ID: 0> #<STORE-OBJECT ID: 1>
;  #<FOO ID: 2> #<TUTORIAL-OBJECT ID: 3>
;  #<TUTORIAL-OBJECT ID: 4> #<TUTORIAL-OBJECT ID: 5>)

;;; In order to change the slot values of persistent object, the
;;; application needs to be in a transaction context.  This can be
;;; done either by invoking a named transaction as above, or by
;;; creating an anonymous transaction.  In an anonymous transaction,
;;; all write accesses to persistent objects are logged.

(define-persistent-class tutorial-object2 ()
  ((b :update)))

(make-instance 'tutorial-object2 :b 3)
; executing transaction #<TRANSACTION 21.04.2008 08:03:27 MAKE-INSTANCE TUTORIAL-OBJECT2 ID 6 B 3> at timestamp 3417746607
; => #<TUTORIAL-OBJECT2 ID: 6>
(setf (slot-value (store-object-with-id 6) 'b) 4)
; => Error
; Attempt to set persistent slot B of #<TUTORIAL-OBJECT2 ID: 6> outside of a transaction
(with-transaction ()
  (setf (slot-value (store-object-with-id 6) 'b) 4))
; => 4
(tutorial-object2-b (store-object-with-id 6))
; => 4

;;;## Object creation and deletion protocol

;;; Persistent objects have the metaclass `PERSISTENT-CLASS'.  They
;;; are created using `MAKE-INSTANCE' just like any other CLOS object.
;;; The datastore allocates a unique ID for the object and initializes
;;; it using the usual CLOS protocol.  In order to perform any
;;; additional actions during initialization of the persistent
;;; instance, the `INITIALIZE-INSTANCE' function must be specialized
;;; for the persistent class.  In addition, the datastore calls the
;;; `INITIALIZE-TRANSIENT-INSTANCE' for the new object when it is
;;; initially created as well as when it is restored from the
;;; transaction log or snapshot file.  It may be used to initialize
;;; transient aspects of the objects, but it must not alter the
;;; persistent state of the object.  `INITIALIZE-TRANSIENT-INSTANCE'
;;; does not have access to the initargs used to initially create the
;;; persistent instance.
;;;
;;; We can define the following class with a transient and a
;;; persistent slot.

(define-persistent-class protocol-object ()
  ((a :update :transient t)
   (b :update)))

;;; We can modify the slot `A' outside a transaction:
(make-instance 'protocol-object :a 1 :b 2)
; executing transaction #<TRANSACTION 21.04.2008 08:10:49 MAKE-INSTANCE PROTOCOL-OBJECT ID 7 A 1 B 2> at timestamp 3417747049
; => #<PROTOCOL-OBJECT ID: 7>
(setf (protocol-object-a (store-object-with-id 7)) 2)
; => 2

;;; However, we cannot modify the slot `B', as it is persistent and
;;; has to be changed inside a transaction.

(setf (protocol-object-b (store-object-with-id 7)) 4)
; => Error
; Attempt to set persistent slot B of #<PROTOCOL-OBJECT ID: 7> outside of a transaction

;;; An object can be removed from the datastore using the transaction
;;; `DELETE-OBJECT', which calls the method `DESTROY-OBJECT' on the
;;; object. Special actions at deletion time have to be added by
;;; overriding `DESTROY-OBJECT'. The basic action is to remove the
;;; object from all its indices.

;;;## Snapshotting an object datastore

;;; We can snapshot the persistent state of all created objects by
;;; using `SNAPSHOT'.
(snapshot)
; Snapshotting subsystem #<STORE-OBJECT-SUBSYSTEM #xE54991E> of #<MP-STORE DIR: "/tmp/object-store/">
; Successfully snapshotted #<STORE-OBJECT-SUBSYSTEM #xE54991E> of #<MP-STORE DIR: "/tmp/object-store/">

;;; This will create a backup directory containing the old transaction
;;; log, and the creation of a snapshot file in the "current"
;;; directory.

(directory "/tmp/object-store/**/*.*")
; => (#P"/tmp/object-store/20080421T061210/random-state"
;     #P"/tmp/object-store/20080421T061210/transaction-log"
;     #P"/tmp/object-store/current/random-state"
;     #P"/tmp/object-store/current/store-object-subsystem-snapshot"
;     #P"/tmp/object-store/current/transaction-log")

;;; The snapshot file contains all persistent objects present at
;;; snapshotting time, and the value of their persistent
;;; slots. Further transaction are recorded in a new transaction log.

;;;## Adding indices to store objects

;;; The object datastore builds upon the functionality of the BKNR
;;; indices system. All store objects are of the metaclass
;;; `INDEXED-CLASS', so adding indices is seamless. Indices are
;;; transient, and are rebuilt every time the datastore is
;;; restored. Adding an index on a transient slot or on a persistent
;;; slot makes no difference.

(define-persistent-class gorilla ()
  ((name :read :index-type string-unique-index
               :index-reader gorilla-with-name
               :index-values all-gorillas)
   (mood :read :index-type hash-index
               :index-reader gorillas-with-mood
               :index-keys all-gorilla-moods)))

(make-instance 'gorilla :name "lucy" :mood :aggressive)
; => #<GORILLA ID: 8>
(make-instance 'gorilla :name "john" :mood :playful)
; => #<GORILLA ID: 9>
(make-instance 'gorilla :name "peter" :mood :playful)
; => #<GORILLA ID: 10>
(gorilla-with-name "lucy")
; => #<GORILLA ID: 8>
(gorillas-with-mood :playful)
; => (#<GORILLA ID: 10> #<GORILLA ID: 9>)

;;;## Adding blobs

;;; A blob is a Binary Large OBject, that means it is a normal
;;; persistent object with an associated binary data (that most of the
;;; time is quite large). The object datastore supports storing this
;;; large binary data outside the transaction log and the snapshot
;;; file in order not to strain the store memory footprint too much,
;;; and to be able to access the binary data from outside the LISP
;;; session. This can be useful in order to copy the binary data using
;;; the operating system calls directly. Blobs are used to store
;;; images in the BKNR Web Framework (in fact, eboy.com contains more
;;; than 40000 images). They have also been used to store MP3 files
;;; for the GPN interactive DJ.
;;;
;;; In addition to the binary data, a blob object also holds a `TYPE'
;;; and a `TIMESTAMP'. The type of a blob object is a keyword somehow
;;; identifying the type of binary data it stores. For example, for
;;; the images of the eboy datastore, we used the keywords `:JPEG',
;;; `:PNG', `:GIF' to identity the different file formats used to
;;; store images. The timestamp identifies the time of creation of the
;;; blob object (this can be useful to cache binary data of blob
;;; objects in a web server context).
;;;
;;; Stores are implemented in a custom subsystem, which takes as key
;;; argument `:DIRECTORY' the name of a directory where the binary
;;; data of the blob objects is stored as a simple file. This
;;; directory can be further partitioned dynamically by the datastore,
;;; when provided with the argument `:N-BLOBS-PER-DIRECTORY'. The
;;; value of this argument is stored in the directory of the
;;; datastore, so that a future instance of the blob subsystem is
;;; initialised correctly.
;;;
;;; We can now add blob support to our existing object datastore by
;;; adding the blob subsystem to its list of subsystems.

(close-store)
(make-instance 'mp-store :directory "/tmp/object-store/"
                         :subsystems (list
                                      (make-instance 'store-object-subsystem)
                                      (make-instance 'blob-subsystem)))

;;; The blob subsystem provides a few functions and transactions to
;;; work with blobs. To show how to use these functions, we will
;;; define a blob class in our example store. A photo is simply a
;;; binary object with a name.

(define-persistent-class photo (blob)
  ((name :read)))

;;; A blob can be created using the function `MAKE-BLOB-FROM-FILE',
;;; which is a wrapper around `MAKE-INSTANCE' and the function
;;; `BLOB-FROM-FILE'. The method `BLOB-FROM-FILE' fills the binary
;;; data of a blob object by reading the content of a file. This
;;; binary data is then stored in a file named after the ID of the
;;; object in the blob root directory of the blob subsystem.

(make-blob-from-file "/tmp/bla.jpg" 'photo :name "foobar"
                                           :type :jpg)
; => #<PHOTO ID: 11, TYPE: jpg>

;;; We can work with the photo object in the same way as when we work
;;; with a normal object. However, we can access the binary data using
;;; the methods `BLOB-PATHNAME', which returns the pathname to the
;;; file in the blob root that holds the binary data of the
;;; object.

(blob-pathname (store-object-with-id 11))
; => #P"/tmp/object-store/blob-root/11"

;;; The method `BLOB-TO-FILE' and `BLOB-TO-STREAM' write the binary
;;; data of the object to the specified file or stream (the stream has
;;; to be of the type `(UNSIGNED-BYTE 8)'). The macro `WITH-OPEN-BLOB'
;;; is provided as wrapper around the `WITH-OPEN-FILE' macro.

;;;## Relaxed references

;;; It sometimes happens that a persistent object is deleted while it
;;; still is referenced by another object. This can lead to problems
;;; when snapshotting and restoring the datastore, as the referenced
;;; object is not available anymore.
;;;
;;; When a slot is specified as being a relaxed object reference slot
;;; using the slot option `:RELAXED-OBJECT-REFERENCE', a reference to
;;; an unexistent object can be encoded during snapshot. The object
;;; subsystem issues a warning when a reference to a non-existent
;;; object is encoded. When a reference to a deleted object is decoded
;;; form the snapshot file, a `NIL' value is returned if the slot from
;;; where the object is referenced supports relaxed references. Else,
;;; an error is thrown.

(define-persistent-class relaxed-object ()
  ((a :update :relaxed-object-reference t)))

(make-instance 'relaxed-object)
; => #<RELAXED-OBJECT ID: 12>
(make-instance 'relaxed-object)
; => #<RELAXED-OBJECT ID: 13>
(with-transaction ()
  (setf (slot-value (store-object-with-id 12) 'a) (store-object-with-id 13)))
; => #<RELAXED-OBJECT ID: 13>
(delete-object (store-object-with-id 13))
; => T
(snapshot)
; Warning: Backup of the datastore in /tmp/object-store/20080421T064811/.
; While executing: (:INTERNAL (SNAPSHOT-STORE (STORE))), in process worker(1750).
; Snapshotting subsystem #<STORE-OBJECT-SUBSYSTEM #xE6069EE> of #<MP-STORE DIR: "/tmp/object-store/">
; Warning: Encoding reference to destroyed object with ID 13 from slot A of object RELAXED-OBJECT with ID 12.
; While executing: #<STANDARD-METHOD ENCODE-OBJECT (STORE-OBJECT T)>, in process worker(1750).
; Successfully snapshotted #<STORE-OBJECT-SUBSYSTEM #xE6069EE> of #<MP-STORE DIR: "/tmp/object-store/">
; Snapshotting subsystem #<BLOB-SUBSYSTEM #xE6069CE> of #<MP-STORE DIR: "/tmp/object-store/">
; Successfully snapshotted #<BLOB-SUBSYSTEM #xE6069CE> of #<MP-STORE DIR: "/tmp/object-store/">
; => NIL
(restore)
; restoring #<MP-STORE DIR: "/tmp/object-store/">
; Restoring the subsystem #<STORE-OBJECT-SUBSYSTEM #xE6069EE> of #<MP-STORE DIR: "/tmp/object-store/">
; loading snapshot file /tmp/object-store/current/store-object-subsystem-snapshot
; Warning: internal inconsistency during restore: can't find store object 13 in loaded store
; While executing: %DECODE-STORE-OBJECT, in process worker(1754).
; Warning: Reference to inexistent object with id 13 from unnamed container, returning NIL.
; While executing: %DECODE-STORE-OBJECT, in process worker(1754).
; Restoring the subsystem #<BLOB-SUBSYSTEM #xE6069CE> of #<MP-STORE DIR: "/tmp/object-store/">
; loading transaction log /tmp/object-store/current/transaction-log
; executing transaction #<ANONYMOUS-TRANSACTION 21.04.2008 08:48:48 PREPARE-FOR-SNAPSHOT NIL> at timestamp 3417749328
(relaxed-object-a (store-object-with-id 12))
; => NIL

;;;# Store internals

;;;## Binary data files
;;;
;;; This implementation of the BKNR datastore uses a binary encoding
;;; of Lisp data. The encoding library is used by both the transaction
;;; system and the object system and is mostly independent of
;;; them. Users need not be aware of the details of this encoding,
;;; except that (1) primitive data stored needs to be supported by the
;;; encoding library and (2) user-defined object systems need to
;;; register their own encoder and decoder methods to allow their
;;; objects to be used as part of transaction arguments.

Function ENCODE (OBJECT STREAM)

Function DECODE (STREAM) =<  OBJECT

;;; The `STREAM' must be specialized on `(unsigned-byte 8)'.

;;; The object store subsystem uses the encoding library to encode the
;;; persistent state of all the objects in the store. It does this by
;;; first serializing the layout of a class (which is a list of
;;; slot-names), then by first serializing the class and the id of
;;; each object, and finally by serializing the slots of each
;;; object. This two-step system is necessary to correctly serialize
;;; circular of forward references.

;;; When the snapshot is loaded, an empty instance of each object is
;;; created, and can be referenced only using the `ID'. After each
;;; object has been instantiated, it can be referenced by another
;;; object. The objects are serialized in the order they have been
;;; created.

;;;## Datastore state

;;; The store always is in one of the following states:

;;; :closed - No store is open, no persistent operations are
;;; possible.  It is an error, which will eventually be signaled, to
;;; execute transactions in this state.

;;; :restore - The store is currently recovering from a restart

;;; :idle - The store has been opened and is prepared to execute
;;; transactions.

;;; :transaction - The store is currently executing a transaction

;;; Note that all transactions are serialized and thus only one
;;; transaction can be active at any time.

;;; :snapshot - The store is currently writing a snapshot.

;;;## Transactions

;;; Transactions are objects of the class `TRANSACTION', and have a
;;; slot containing the symbol of their transaction function, as well
;;; as a list of the arguments that have to be passed to this
;;; function. When a transaction is executed, a timestamp
;;; of the execution time is stored in the object.

;;; Transactions can be grouped by enclosing them with a
;;; with-transaction block.  All subtransactions will be logged to the
;;; transaction log as a group and only re-executed on restore time
;;; when the complete group could be read from the file.

(defun foo-bar ()
  (with-transaction ()
    (invoke-transaction-a)
    (invoke-transaction-b)))

;;; The persistent object store further supports the use of anonymous
;;; transactions by automatically converting instance creation and
;;; slot writes to transaction invocations which are atomically
;;; executed upon restore time.

;;;## Snapshot and restore procedures

;;; When the datastore is snapshotted, the transaction layer ensures
;;; that the store is opened, and that there are subsystems in the
;;; store. Without subsystems, the transaction log is the only way for
;;; the store to achieve persistence, and no snapshot can be made. The
;;; store is then switched to read-only, and a backup directory is
;;; created, containing the current transaction log and previous
;;; snapshot files. This way, the older state of the datastore is not
;;; lost. Then, each subsystem is asked to save its persistent state
;;; by calling the method `SNAPSHOT-SUBSYSTEM'. When an error is
;;; thrown during the snapshot of the subsystems, the backup directory
;;; is renamed to be the current directory, and the store is
;;; reopened.

;;; When the datastore is restored, the store is switched to read
;;; only, and each subsystem is asked to restore its persistent
;;; state. Note that the subsystems are restored in the order in which
;;; they are listed in the `SUBSYSTEMS' slot of the store, so that
;;; dependent subsystems are restored last. When an error is thrown
;;; while restoring the subsystems, the store is closed, and already
;;; opened subsystems are closed using the method
;;; `CLOSE-SUBSYSTEM'. After the restoring of all the subsystems, the
;;; transaction log file is read, and each transaction recorded is
;;; executed. This is where the `UNTIL' parameter comes into
;;; play. Transactions that have been executed after the time of
;;; `UNTIL' are discarded.

;;;## Filesystem syncing

;;; By default, the transaction log file is synced after a transaction
;;; has been executed, so that all the data is correctly written on
;;; disk. However, this can be a major performance stopper when
;;; executing a big batch of transactions (for example, deleting a few
;;; thousands objects). You can disable the mandatory syncing by
;;; executing your transactions inside the form `WITHOUT-SYNC'.

(without-sync ()
  (execute-a-lot-of-transactions))

;;;## Snapshotting and restoring the object subsystem

;;; Snapshotting and restoring the object subsystem is a bit tricky,
;;; as additional systems come into play. When the object subsystem is
;;; snapshotted using the method `SNAPSHOT-SUBSYSTEM', a snapshot file
;;; containing a binary dump of all the current store objects is
;;; created. First, the layouts of the objects (the name of their
;;; slots) is stored, and minimal information about each object is
;;; stored in the order of their creation (the minimal information
;;; consists of the class of the object, and its ID). After this
;;; information has been stored for each object, the slot values of
;;; the store objects (again in the order of their creation) are
;;; stored in the snapshot file. These two phases are necessary to
;;; allow the snapshotting of circular or forward-referencing
;;; structures.

;;; When the object subsystem is restored, all the indices for classes
;;; contained in the store are cleared in order to accomodate for the
;;; new objects. Be very careful when using class indices that are not
;;; related to store objects. The ID counter of the store subsystem is
;;; reset to 0, and the class-layouts are read from the snapshot
;;; file. Then, the minimal information for each object is read, and
;;; an "empty version" of each object is instantiated. Thus, the
;;; objects can be referenced by their ID. Then, the slot values for
;;; each object are read from the snapshot file, references are
;;; resolved (check the section about relaxed references). Finally,
;;; after each slot value has been set, the method
;;; `INITIALIZE-TRANSIENT-INSTANCE' is called for each created
;;; object.

;;;## Garbage collecting blobs

;;; The binary data of deleted blob objects is kept in the blob root
;;; directory by default. If you want to purge the binary data of
;;; deleted objects, you can use the function
;;; `DELETE-ORPHANED-BLOB-FILES'. However, take note that you won't be
;;; able to restore the persistent state anteriour to the deletion of
;;; the blobs, as their binary data is not stored in the transaction
;;; log and not backed up by the snapshot method of the blob
;;; subsystem.

;;;## Schema evolution in the datastore

;;; The transaction log only stores when a transaction is called, and
;;; with which arguments. However, it doesn't store the definition of
;;; the transaction itself. When the transaction definition is
;;; changed, the transaction log may be restored in a different way,
;;; according to the changes made in the code.
;;;
;;; In the same way, class definition changes are not recorded in the
;;; transaction log. When a class definition is changed (for example a
;;; slot initform is changed), the existing instances of the class are
;;; updated accordingly. However, when the snapshot is restored in a
;;; future session, the objects may be different than those created at
;;; the last restore.
;;;
;;; The only way to cleanly upgrade transaction definitions and class
;;; definitions is to make a snapshot after the changes have been
;;; made. In a future version of the datastore, we hope to store all
;;; the application sourcecode, so that a restore to a certain point
;;; in time does not depend on the latest version of the code. The
;;; object subsystem warns when a class definition is changed, and
;;; urges the developer to make a snapshot of the database. Please be
;;; careful, this can be a pretty tricky source of bugs.

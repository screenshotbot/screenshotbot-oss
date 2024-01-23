(defpackage :bknr.datastore.tests
  (:use #:cl #:bknr.datastore #:bknr.indices
        #:fiveam
        #:fiveam-matchers))
(in-package :bknr.datastore.tests)

(def-suite* :bknr.datastore.tests)

(defun delete-directory (pathname)
  (when (probe-file pathname)
    (fad:delete-directory-and-files pathname)))

(defvar *test-datastore* nil)

(defmacro test-equal (&rest args)
  `(is (equal ,@args)))

(defmacro test-assert (&rest args)
  `(is-true ,@args))

(defmacro test-condition (expr (%quote condition))
  (declare (ignore %quote))
  `(signals ,condition
     ,expr))

(defun make-test-store-directory ()
  ;; bknr.utils:make-temporary-pathname does not really return a new
  ;; directory pathname that is guaranteed to be distinct.  Chances
  ;; that it returns the name of an existing directory are so slim
  ;; that we accept the risk.
  (ensure-directories-exist (format nil "~A/" (bknr.utils:make-temporary-pathname :defaults "/tmp/" :name "store-test"))))

(def-fixture datastore-test-class ()
  (let ((directory (make-test-store-directory))
        error)
    (make-instance 'mp-store :directory directory)
    (unwind-protect
         (handler-bind ((error (lambda (e)
                                 (declare (ignore e))
                                 (setf error t))))
           (&body))
      (close-store)
      (if error
          (format output ";; store directory ~A not deleted~%" directory)
          (delete-directory directory)))))

(defvar *tests* (make-hash-table))

(defun do-run-test (thunk)
  "Run the test in THUNK, then verify that the store contains the
`same' objects after a restore and after snapshot and a restore."
  (let ((bknr.datastore::*store-verbose* nil) initial-objects)
    (funcall thunk)
    (let ((next-object-id (bknr.datastore::next-object-id (bknr.datastore::store-object-subsystem))))
      (setf initial-objects (object-classes-and-ids))
      (restore)
      (test-equal initial-objects (object-classes-and-ids))
      (test-equal next-object-id (bknr.datastore::next-object-id (bknr.datastore::store-object-subsystem)))
      (snapshot)
      (restore)
      (test-equal initial-objects (object-classes-and-ids))
      (test-equal next-object-id (bknr.datastore::next-object-id (bknr.datastore::store-object-subsystem))))))

(defmacro defdstest (name args &body body)
  (when args
    (error "unexpected arguments ~A to defdstest ~A" args name))
  `(test ,name
     (with-fixture datastore-test-class ()
       ,@body)))

(defdstest store-setup ()
  (test-assert *store*))

(defdstest create-object ()
  (let ((obj (make-instance 'store-object)))
    (test-assert obj)
    (test-equal (list obj) (all-store-objects))))

(defdstest create-multiple-objects ()
  (let ((o1 (make-instance 'store-object))
        (o2 (make-instance 'store-object)))
    (test-assert o1)
    (test-assert o2)
    (test-equal (length (all-store-objects)) 2)
    (test-assert (subsetp (list o1 o2) (all-store-objects)))))

(defdstest delete-multiple-objects ()
  (let ((o1 (make-instance 'store-object))
        (o2 (make-instance 'store-object)))
    (test-assert o1)
    (test-assert o2)
    (test-equal (length (all-store-objects)) 2)
    (test-assert (subsetp (list o1 o2) (all-store-objects)))
    (delete-object o1)
    (test-equal (all-store-objects) (list o2))
    (delete-object o2)
    (test-equal (all-store-objects) nil)))

(defdstest restore ()
  (let ((object-id (store-object-id (make-instance 'store-object))))
    (restore)
    (test-equal 1 (length (all-store-objects)))
    (test-equal object-id (store-object-id (first (all-store-objects))))))

(defdstest snapshot-and-restore ()
  (let ((object-id (store-object-id (make-instance 'store-object))))
    (snapshot)
    (restore)
    (test-equal 1 (length (all-store-objects)))
    (test-equal object-id (store-object-id (first (all-store-objects))))))

(defdstest restore-multiple-objects ()
  (dotimes (i 10)
    (make-instance 'store-object))
  (restore)
  (test-equal 10 (length (all-store-objects))))

(defdstest snapshot-restore-multiple-objects ()
  (dotimes (i 10)
    (make-instance 'store-object))
  (snapshot)
  (restore)
  (test-equal (length (all-store-objects)) 10))

(defconstant +stress-size+ 10000)

(defdstest stress-test ()
  (format t "Creating ~A objects in two threads~%" +stress-size+)
  (time (bknr.datastore::without-sync ()
          (labels ((stress ()
                     (dotimes (i +stress-size+)
                       (make-instance 'store-object))))
            (let ((threads (list (bt:make-thread #'stress)
                                 (bt:make-thread #'stress))))
              (loop while (some #'bt:thread-alive-p threads)
                 do (sleep 1))))))
  (test-equal (length (all-store-objects)) (* 2 +stress-size+)))

(defdstest stress-test-2 ()
  (bknr.datastore::without-sync ()
    (format t "Creating ~A objects~%" +stress-size+)
    (time (dotimes (i +stress-size+)
            (make-instance 'store-object)))
    (format t "Deleting ~A objects~%" (length (all-store-objects)))
    (time (map-store-objects #'delete-object))
    (test-equal (all-store-objects) nil)))

(defdstest holes-test ()
  (dotimes (i +stress-size+)
    (let ((delete (zerop (random 2))))
      (with-transaction (:foo)
        (funcall (if delete #'delete-object #'identity)
                 (make-instance 'store-object))))))

(defdstest make-instance-in-anon-txn ()
  (with-transaction ()
    (make-instance 'store-object))
  (restore)
  (test-equal 1 (length (class-instances 'store-object))))

(defdstest make-instance-in-anon-txn-1 ()
  (with-transaction ()
    (test-assert (make-instance 'store-object))))


(define-persistent-class parent ()
  ((child :update :initform nil :initarg nil)))

(define-persistent-class child ()
                         ())

(defclass child-with-index ()
  ((foo :initarg :foo
        :index-type unique-index
        :index-reader child-by-foo))
  (:metaclass persistent-class))

(defun object-classes-and-ids ()
  "Return a list of conses with the car being a class name and the cdr
  being the object id for all persistent objects in the store"
  (sort (mapcar (lambda (object)
                  (cons (class-name (class-of object))
                        (store-object-id object)))
                (all-store-objects))
        #'< :key #'cdr))

(defdstest make-referenced-object-in-anon-tx ()
  (with-transaction (:make)
    (make-instance 'parent :child (make-instance 'child))))

(defdstest circular-in-anon-txn-without-serialization ()
  (let ((parent (make-instance 'parent)))
    (with-transaction (:circular)
      (setf (parent-child parent) (make-instance 'child))))
  (test-equal (find-class 'child)
              (class-of (parent-child (first (class-instances 'parent))))))

(defdstest serialize-circular-in-anon-txn ()
  (let ((parent (make-instance 'parent)))
    (with-transaction (:circular)
      (setf (parent-child parent) (make-instance 'child))))
  (restore)
  (test-equal (find-class 'child)
              (class-of (parent-child (first (class-instances 'parent))))))

(defdstest serialize-self-circular-in-anon-txn ()
  (let ((object (make-instance 'parent)))
    (with-transaction (:circular)
      (setf (parent-child object) object)))
  (restore)
  (let ((object (first (class-instances 'store-object))))
    (test-assert object)
    (test-equal object (parent-child object)))
  (snapshot)
  (restore)
  (let ((object (first (class-instances 'store-object))))
    (test-assert object)
    (test-equal object (parent-child object))))

(defdstest delete-object-in-anon-txn ()
  (let (object)
    (with-transaction (:make)
      (setf object (make-instance 'child)))
    (with-transaction (:delete)
      (delete-object object))
    (restore)
    (test-assert (object-destroyed-p object))))

(defdstest delete-object-and-check-object-id-of-next-1 ()
  (let (object-id)
    (with-transaction (:make)
      (let ((object (make-instance 'store-object)))
        (setf object-id (store-object-id object))
        (delete-object object)))
    (restore)
    (test-assert (< object-id (store-object-id (make-instance 'store-object))))))

(defdstest delete-object-and-check-object-id-of-next-2 ()
  (let (object-id)
    (with-transaction (:make)
      (let ((object (make-instance 'store-object)))
        (setf object-id (store-object-id object))))
    (snapshot)
    (restore)
    (test-assert (< object-id (store-object-id (make-instance 'store-object))))))

(defdstest delete-object-and-check-object-id-of-next-3 ()
  (let (object-id)
    (with-transaction (:make)
      (let ((object (make-instance 'store-object)))
        (setf object-id (store-object-id object))
        (delete-object object)))
    (snapshot)
    (restore)
    (test-assert (< object-id (store-object-id (make-instance 'store-object))))))

(define-persistent-class class-with-transient-slot ()
  ((slot :update
         :transient t
         :initform 0)))

(defdstest test-transient-slots ()
  (let ((object-id (store-object-id (make-instance 'class-with-transient-slot))))
    (restore)
    (test-equal 0 (class-with-transient-slot-slot (find-store-object object-id)))
    (setf (class-with-transient-slot-slot (find-store-object object-id)) 1)
    (restore)
    (test-equal 0 (class-with-transient-slot-slot (find-store-object object-id)))
    (snapshot)
    (restore)
    (test-equal 0 (class-with-transient-slot-slot (find-store-object object-id)))))

(define-persistent-class persistent-mixin ()
  ((mixin-slot :update
               :initform 2)))

(define-persistent-class inherit-multiple (persistent-mixin parent)
  ())

(defdstest multiple-inheritance-test ()
  (let* ((o1 (make-instance 'inherit-multiple :child (make-instance 'child)))
         (o2 (make-instance 'inherit-multiple :child o1)))
    (test-equal o1 (parent-child o2))))

(defdstest abort-anonymous-transaction ()
  (let ((parent (make-instance 'parent :child nil)))
    (ignore-errors
      (with-transaction (:abort)
        (setf (parent-child parent) (make-instance 'child))
        (error "abort")))
    ;; The old behavior was to to abort the whole transaction, the new
    ;; one will still play out any intermediate transactions.
    (is-true (parent-child parent))
    (assert-that (class-instances 'child)
                 (has-length 1))))

(defdstest ensure-setf-slot-value-returns-newval ()
  (let ((parent (make-instance 'parent :child nil)))
    (let ((child (setf (parent-child parent) (make-instance 'child))))
      (is (eql child (car (class-instances 'child)))))))

#+nil
(defdstest abort-anonymous-transaction-for-indices ()
  (let ((parent (make-instance 'parent :child nil)))
    (ignore-errors
      (with-transaction (:abort)
        (setf (parent-child parent) (make-instance 'child-with-index :foo 2))
        (error "abort")))
    (test-equal nil (parent-child parent))
    (test-equal nil (class-instances 'child-with-index))
    ;; This fails: Even though we've rolled back the creation of
    ;; object, we haven't rolled back the indices to their previous
    ;; state. -- Arnold
    #+nil
    (test-equal nil (child-by-foo 2))))

(bknr.datastore:deftransaction tx-set-child (parent child)
  (setf (parent-child parent) child))

(defdstest set-slot-from-within-txn ()
  (let ((parent (make-instance 'parent))
        (child (make-instance 'child)))
    (tx-set-child parent child)
    (is (eql child (parent-child parent)))
    (restore)
    (is (eql child (parent-child parent)))))


(defclass object-with-init (store-object)
  ((arg :initarg :arg
        :reader arg))
  (:metaclass persistent-class)
  (:default-initargs :arg (+ 1 1)))

(deftransaction tx-make-object ()
  (make-instance 'object-with-init))

(deftransaction tx-make-object-with-arg (arg)
  (make-instance 'object-with-init :arg arg))

(defdstest default-initargs-is-parsed ()
  (let ((obj (make-instance 'object-with-init)))
    (is (eql 2 (arg obj))))
  (let ((obj (tx-make-object)))
    (is (eql 2 (arg obj))))
  (assert-that (mapcar #'arg (class-instances 'object-with-init))
               (contains 2 2))
  (restore)
  (assert-that (mapcar #'arg (class-instances 'object-with-init))
               (contains 2 2)))

(defdstest default-initargs-doesnt-ignore-args ()
  (let ((obj (make-instance 'object-with-init :arg 5)))
    (is (eql 5 (arg obj))))
  (let ((obj (tx-make-object-with-arg 5)))
    (is (eql 5 (arg obj))))
  (assert-that (mapcar #'arg (class-instances 'object-with-init))
               (contains 5 5))
  (restore)
  (assert-that (mapcar #'arg (class-instances 'object-with-init))
               (contains 5 5)))

(defclass object-with-ensure-init (store-object)
  ((arg :initarg :arg
        :reader arg))
  (:metaclass persistent-class)
  (:default-initargs :arg (error "must specify :arg")))

(deftransaction tx-make-object-with-ensure-init (arg)
  (make-instance 'object-with-ensure-init :arg arg))

(defdstest default-initargs-with-overrident-must-behave ()
  (let ((obj (make-instance 'object-with-ensure-init :arg 5)))
    (is (eql 5 (arg obj))))
  (let ((obj (tx-make-object-with-ensure-init 5)))
    (is (eql 5 (arg obj))))

  (assert-that (mapcar #'arg (class-instances 'object-with-ensure-init))
               (contains 5 5))
  (restore)
  (assert-that (mapcar #'arg (class-instances 'object-with-ensure-init))
               (contains 5 5))
  (signals error
    (make-instance 'object-with-ensure-init))
  (assert-that (mapcar #'arg (class-instances 'object-with-ensure-init))
               (contains 5 5)))

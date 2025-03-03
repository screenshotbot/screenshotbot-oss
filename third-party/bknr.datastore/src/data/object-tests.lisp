(defpackage :bknr.datastore.tests
  (:use #:cl #:bknr.datastore #:bknr.indices
        #:fiveam
        #:fiveam-matchers)
  (:import-from #:bknr.datastore
                #:persistent-effective-slot-definition
                #:%id-cache
                #:*in-restore-p*
                #:next-object-id
                #:store-object-subsystem
                #:with-store-state
                #:encode
                #:%encode-string
                #:%encode-integer
                #:encode-slots-for-object
                #:make-object-snapshot
                #:class-layout-slots
                #:class-layout
                #:encode-create-object
                #:%log-crash)
  (:import-from #:bknr.indices
                #:base-indexed-object)
  (:import-from #:fiveam-matchers/core
                #:error-with-string-matching
                #:signals-error-matching)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex)
  (:import-from #:fiveam-matchers/described-as
                #:described-as))
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
          (format t ";; store directory ~A not deleted~%" directory)
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

(defdstest read-object-id ()
  (let ((obj (make-instance 'store-object)))
    (is (eql 0 (store-object-id obj))))
  (let ((obj (make-instance 'store-object)))
    (is (eql 1 (store-object-id obj)))))

(defdstest find-by-object-id ()
  (let ((obj1 (make-instance 'store-object))
        (obj2 (make-instance 'store-object)))
    (is (eql obj2 (store-object-with-id 1)))
    (is (eql obj1 (store-object-with-id 0)))))

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

(defclass child-with-index (store-object)
  ((foo :initarg :foo
        :index-type unique-index
        :accessor %foo
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

(defdstest delete-indexed-object ()
  (let ((obj (make-instance 'child-with-index
                            :foo :bar)))
    (is (eql obj (child-by-foo :bar)))
    (delete-object obj)
    (is (eql nil (child-by-foo :bar)))))

(defdstest set-indexed-slot-in-object ()
  (let ((obj (make-instance 'child-with-index
                            :foo :bar)))
    (is (eql obj (child-by-foo :bar)))
    (setf (%foo obj) :car)
    (is (eql nil (child-by-foo :bar)))
    (is (eql obj (child-by-foo :car)))))

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

(define-condition fake-error (error)
  ())

(defdstest %log-crash-happy-path ()
  (signals fake-error ;; as opposed to any errors from log-crash
   (handler-bind ((error #'%log-crash))
     (error 'fake-error))))


(defdstest encode-create-object-updates-the-class-layouts ()
  (let ((class-layouts (make-hash-table)))
    (let ((stream (flex:make-in-memory-output-stream)))
      (encode-create-object class-layouts
                            ;; the class-name doesn't matter here
                            (make-instance 'object-with-init)
                            stream)
      (assert-that
       (alexandria:hash-table-keys class-layouts)
       (contains (find-class 'object-with-init)))
      (let ((layouts (alexandria:hash-table-values class-layouts)))
        (assert-that
         layouts
         (contains
          (has-typep 'class-layout)))
        (assert-that
         (class-layout-slots (first layouts))
         (contains 'arg 'bknr.datastore::last-change))))))

(defclass object-with-relaxed-slot (store-object)
  ((reference :initarg :reference
              :relaxed-object-reference t)
   (other-reference :initarg :other-reference))
  (:metaclass persistent-class))

(defdstest can-save-when-reference-is-killed ()
  (let ((inner (make-instance 'object-with-init)))
    (make-instance 'object-with-relaxed-slot
                   :reference inner)
    (delete-object inner)
    (finishes
      (snapshot))))

(defdstest but-cannot-save-when-other-reference-is-killed ()
  (let ((inner (make-instance 'object-with-init)))
    (make-instance 'object-with-relaxed-slot
                   :other-reference inner)
    (delete-object inner)
    (signals error
      (snapshot))))

(defclass async-object-with-slot (store-object)
  ((slot1 :initarg :slot1)
   (slot2 :initarg :slot2))
  (:metaclass persistent-class))

(defclass async-object-snapshot ()
  ((original :initarg :original)))

(defmethod make-object-snapshot ((self async-object-with-slot))
  (make-instance 'async-object-snapshot :original self))

(defmethod encode-slots-for-object (class-layout (self async-object-snapshot)
                                    stream)
  (assert (equal '(bknr.datastore::last-change slot1 slot2)
                 (class-layout-slots class-layout)))
  (encode 20 stream)
  (encode "bar" stream)
  (encode 45 stream))

(defdstest asynchronously-save-object ()
  (make-instance 'async-object-with-slot
                 :slot1 "foo"
                 :slot2 22)
  (snapshot)
  (restore)
  (let ((result (first (bknr.datastore:class-instances 'async-object-with-slot))))
    (is (equal "bar" (slot-value result 'slot1)))
    (is (equal 45 (slot-value result 'slot2)))))


(defdstest cant-finalize-class-without-store-object ()
  (signals bknr.datastore::must-inherit-store-object
    (eval
     `(defclass foo-dfwersfsdfds (base-indexed-object)
        ()
        (:metaclass persistent-class)))))

;; This test causes SBCL to crash and burn
#+nil
(defdstest cant-finalize-class-without-any-base-class-at-all ()
  (sleep 2)
  (signals bknr.datastore::must-inherit-store-object
    (eval
     `(defclass foo-dfwersfsdfdsdfdf ()
        ()
        (:metaclass persistent-class)))))


(defdstest make-instance-fails-while-restore-is-in-progress-on-another-thread ()
  (with-store-state (:restore)
    (let ((*in-restore-p* nil))
      (is (equal 0 (next-object-id (store-object-subsystem))))
      (signals-error-matching ()
         (make-instance 'object-with-init)
         (error-with-string-matching
          (matches-regex ".*Restore is in progress.*")))
      (is (equal 0 (next-object-id (store-object-subsystem)))))))

(defdstest set-slot-value-fails-while-restore-is-in-progress-on-another-thread ()
  (with-store-state (:restore)
    (is (equal 0 (next-object-id (store-object-subsystem))))
    (let ((obj (let ((*in-restore-p* t)) (make-instance 'object-with-init))))
      (finishes
        (let ((*in-restore-p* t))
          (setf (slot-value obj 'arg) 2)))

      (let ((*in-restore-p* nil))
        (signals-error-matching ()
          (setf (slot-value obj 'arg) 3)
          (error-with-string-matching
           (matches-regex ".*Restore is in progress.*"))))
      (is (eql 2 (slot-value obj 'arg))))
    (is (equal 1 (next-object-id (store-object-subsystem))))))

(defdstest can-access-id-cache-without-issues ()
  (let ((obj (make-instance 'parent)))
    (setf (%id-cache obj) 22)
    (is (eql 22 (%id-cache obj)))

    (delete-object obj)
    (is (eql 22 (%id-cache obj)))
    (setf (%id-cache obj) 23)
    (is (eql 23 (%id-cache obj)))))

(defdstest check-id-cache-slot-type ()
  (let ((obj (make-instance 'parent)))
    (setf (%id-cache obj) 22)
    (is (eql 22 (%id-cache obj)))

    (let ((slot (loop for slot in                       
                               (closer-mop:class-slots (find-class 'parent))
                      if (eql '%id-cache (closer-mop:slot-definition-name slot))
                        return slot)))
      (assert-that (type-of slot)
                   (described-as "ha! it's not standard-slot-definition, deal with it."
                     ;; ^ but Lispworks still seems to optimize the accessors
                     (is-equal-to 'persistent-effective-slot-definition))))

    (delete-object obj)
    (is (eql 22 (%id-cache obj)))
    (is (eql 22 (slot-value obj '%id-cache)))
    (setf (%id-cache obj) 23)
    (is (eql 23 (%id-cache obj)))
    (setf (slot-value obj '%id-cache) 34)
    (is (eql 34 (%id-cache obj)))))

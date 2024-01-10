;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-permissive-persistent-class
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/permissive-persistent-class
                #:value-map
                #:permissive-persistent-class)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:delete-object
                #:store-object)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that)
  (:import-from #:bknr.indices
                #:unique-index))
(in-package :util/store/test-permissive-persistent-class)

(util/fiveam:def-suite)

(defclass my-object (store-object)
  ((arg :initarg :arg
        :accessor arg))
  (:metaclass permissive-persistent-class))

(defclass my-object-old (store-object)
  ((arg :initarg :arg
        :accessor arg))
  (:metaclass persistent-class))

(def-fixture state ()
  (with-test-store ()
    (&body)))

(defmacro test* (name () &body body)
  `(test ,name
     (with-fixture state ()
       ,@body)))

(test* simple-create-etc ()
  (finishes
    (make-instance 'my-object))
  (finishes
    (delete-object (make-instance 'my-object))))

(test* set-slot ()
  (let ((obj (make-instance 'my-object :arg "foo")))
    (is (equal "foo" (arg obj)))
    (setf (arg obj) "bleh")
    (is (equal "bleh" (arg obj)))))

(test* must-be-a-hash-table ()
  (let ((obj (make-instance 'my-object)))
    (is (hash-table-p (value-map obj)))))

(test* set-a-slot-value ()
  (let ((obj (make-instance 'my-object)))
    (setf (arg obj) "foo")
    (is (equal "foo" (gethash "ARG" (value-map obj))))))

(test* simple-snapshot ()
  (let ((obj (make-instance 'my-object)))
    (finishes
      (util:safe-snapshot))))


(defclass two-slots-with-same-name (store-object)
  ((cl-user::arg :accessor arg1)
   (arg :accessor arg2))
  (:metaclass permissive-persistent-class))

(test* different-slot-with-same-symbol-name-have-the-same-value ()
  (let ((obj (make-instance 'two-slots-with-same-name)))
    (setf (arg1 obj) "hello")
    (is (equal "hello" (arg2 obj)))))

(defclass two-slots-with-%-name (store-object)
  ((arg :accessor arg1)
   (%arg :accessor arg2))
  (:metaclass permissive-persistent-class))

(test* different-slot-with-%-prefix ()
  (let ((obj (make-instance 'two-slots-with-%-name)))
    (setf (arg1 obj) "hello")
    (is (equal "hello" (arg2 obj)))))

(test reload-object
  (tmpdir:with-tmpdir (dir)
    (with-test-store (:dir dir)
      (make-instance 'my-object :arg "car")
      (util:safe-snapshot))
    (assert-that (bknr.datastore:all-store-objects)
                 (has-length 0))
    (with-test-store (:dir dir)
      (assert-that (bknr.datastore:all-store-objects)
                   (has-length 1))
      (assert-that (arg (car (bknr.datastore:all-store-objects)))
                   (is-equal-to "car")))))

(test* makunbound ()
  (let ((obj (make-instance 'my-object :arg "car")))
    (slot-makunbound obj 'arg)
    (is-false (slot-boundp obj 'arg))
    (is (equal 0 (hash-table-count (value-map obj))))))

(test saves-transactions
  (tmpdir:with-tmpdir (dir)
    (with-test-store (:dir dir)
      (let ((obj (make-instance 'my-object :arg "car")))
        (util:safe-snapshot)
        (setf (arg obj) "foo")))
    (assert-that (bknr.datastore:all-store-objects)
                 (has-length 0))
    (with-test-store (:dir dir)
      (assert-that (bknr.datastore:all-store-objects)
                   (has-length 1))
      (assert-that (arg (car (bknr.datastore:all-store-objects)))
                   (is-equal-to "foo")))))


#+nil ;; Test is broken, bug in core bknr.datastore
(test saves-transactions-for-makunbound
  (tmpdir:with-tmpdir (dir)
    (with-test-store (:dir dir)
      (let ((obj (make-instance 'my-object :arg "car")))
        (util:safe-snapshot)
        (slot-makunbound obj 'arg)))
    (assert-that (bknr.datastore:all-store-objects)
                 (has-length 0))
    (with-test-store (:dir dir)
      (let ((obj (car (bknr.datastore:all-store-objects))))
        (is-false (slot-boundp obj 'arg))))))

#+nil ;; Test is broken, bug in core bknr.datastore
(test saves-transactions-for-makunbound-on-regular-persistent-class
  (tmpdir:with-tmpdir (dir)
    (with-test-store (:dir dir)
      (let ((obj (make-instance 'my-object-old :arg "car")))
        (util:safe-snapshot)
        (slot-makunbound obj 'arg)))
    (assert-that (bknr.datastore:all-store-objects)
                 (has-length 0))
    (with-test-store (:dir dir)
      (let ((obj (car (bknr.datastore:all-store-objects))))
        (is-false (slot-boundp obj 'arg))))))

(defclass obj-with-index (store-object)
  ((arg :initarg :arg
        :accessor arg
        :index-type unique-index
        :index-initargs (:test #'equal)
        :index-reader obj-for-arg))
  (:metaclass permissive-persistent-class))

(test* indices-still-works ()
  (let ((obj (make-instance 'obj-with-index :arg "foo")))
    (is (eql obj (obj-for-arg "foo")))
    (setf (arg obj) "bar")
    (is (eql obj (obj-for-arg "bar")))
    (is-false (obj-for-arg "foo"))))

(defvar *counter*)

(defun incf-counter ()
  (incf *counter*))

(defclass obj-with-initform (store-object)
  ((arg :initarg :arg
        :initform (incf-counter)
        :accessor arg))
  (:metaclass permissive-persistent-class))

(test* obj-with-initform ()
  (let ((*counter* 1))
    (let ((obj (make-instance 'obj-with-initform)))
      (is (eql (arg obj) 2)))))

(defclass old-obj-with-initform (store-object)
  ((arg :initarg :arg
        :initform (progn
                    (incf *counter*))
        :accessor arg))
  (:metaclass persistent-class))

(test* old-obj-with-initform ()
  "This is just making sure that the standard persistent-class only
calls the initform once. (We were seeing a bug with the
permissive-persistent-class showing the initform twice.)"
  (let ((*counter* 1))
    (let ((obj (make-instance 'old-obj-with-initform)))
      (is (> (arg obj) 1))
      (is (= (arg obj) 2)))))

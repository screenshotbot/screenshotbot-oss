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
                #:delete-object
                #:store-object)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that))
(in-package :util/store/test-permissive-persistent-class)

(util/fiveam:def-suite)

(defclass my-object (store-object)
  ((arg :initarg :arg
        :accessor arg))
  (:metaclass permissive-persistent-class))

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

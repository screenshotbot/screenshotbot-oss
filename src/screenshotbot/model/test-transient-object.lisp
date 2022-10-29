;;;; -*- coding: utf-8 -*-
;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-transient-object
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/model/transient-object
                #:with-transient-copy)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:util/object-id
                #:oid-array
                #:object-with-oid)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:does-not-have-item
                #:has-item)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-transient-object)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(with-transient-copy (mem-obj abstract-obj)
  (defclass obj (store-object)
    ((value :initarg :value
            :accessor obj-value))
    (:metaclass persistent-class)))

(test preconditions
  (with-fixture state ()
    (make-instance 'obj)
    (make-instance 'mem-obj)
    (let ((obj (make-instance 'mem-obj)))
      (is (typep obj 'abstract-obj))
      (setf (obj-value obj) :arg)
      (is (eql :arg (obj-value obj))))
    (let ((obj (make-instance 'obj)))
      (is (typep obj 'abstract-obj))
      (signals error
        (setf (obj-value obj) :arg))
      (with-transaction ()
        (setf (obj-value obj) :arg2))
      (is (eql :arg2 (obj-value obj))))))

(test transactions-are-okay-even-for-transient-objects
  (with-fixture state ()
    (let ((obj (make-instance 'mem-obj)))
      (with-transaction ()
        (setf (obj-value obj) :arg2))
      (pass))))

(with-transient-copy (mem-obj-id abstract-obj-id)
  (defclass obj-id (object-with-oid)
    ((value :initarg :value
            :accessor obj-value))
    (:metaclass persistent-class)))

(test object-with-oid
  (with-fixture state ()
    (let ((obj (make-instance 'mem-obj-id :oid #(22 22))))
      (is (equalp #(22 22) (oid-array obj))))))

(with-transient-copy (mem-obj-2 abstract-obj-2
                                :extra-transient-slots (some-slot))
  (defclass obj-2 (store-object)
    ((value :initarg :value
            :accessor obj-value))
    (:metaclass persistent-class)))

(test transient-slots
  (with-fixture state ()
    (assert-that (mapcar #'closer-mop:slot-definition-name
                         (closer-mop:class-slots
                          (closer-mop:ensure-finalized
                           (find-class 'mem-obj-2))))
                 (has-item 'some-slot))
    (assert-that (mapcar #'closer-mop:slot-definition-name
                         (closer-mop:class-slots
                          (closer-mop:ensure-finalized
                           (find-class 'abstract-obj-2))))
                 (does-not-have-item 'some-slot))

    (let ((obj (make-instance 'mem-obj-2)))
      (finishes
        (setf (slot-value obj 'some-slot) 2))
      (is (eql 2 (slot-value obj 'some-slot))))))

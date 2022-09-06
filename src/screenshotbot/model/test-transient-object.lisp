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

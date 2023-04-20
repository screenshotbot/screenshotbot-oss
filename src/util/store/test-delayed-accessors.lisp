;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-delayed-accessors
  (:use #:cl
        #:fiveam)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/delayed-accessors
                #:def-delayed-accessor))
(in-package :util/store/test-delayed-accessors)

(util/fiveam:def-suite)

(defclass my-obj (store-object)
  ((my-slot :accessor %my-slot))
  (:metaclass persistent-class))

(def-delayed-accessor my-slot %my-slot)

(def-fixture state ()
  (with-test-store ()
    (let ((obj (make-instance 'my-obj)))
     (&body))))

(test simple-test
  (with-fixture state ()
    (setf (my-slot obj) 2)
    (is (eql 2 (my-slot obj)))
    (setf (my-slot obj) 3)
    (is (eql 3 (my-slot obj)))))

(test simple-flush
  (with-fixture state ()
    (setf (my-slot obj) 2)
    (is (not (slot-boundp obj 'my-slot)))
    (flush-my-slot)
    (is (eql 2 (my-slot obj)))
    (is (eql 2 (%my-slot obj)))
    (is (eql nil *my-slot-cache*))))

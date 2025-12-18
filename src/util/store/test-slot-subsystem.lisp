;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-slot-subsystem
  (:use #:cl
        #:fiveam)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/store/slot-subsystem
                #:externalized-slot-value))
(in-package :util/store/test-slot-subsystem)


(util/fiveam:def-suite)

(defclass fake-class (store-object)
  ()
  (:metaclass persistent-class))

(def-fixture state ()
  (with-test-store ()
    (let ((obj (make-instance 'fake-class)))
      (&body))))

(test preconditions
  (with-fixture state ()
    (pass)))

(test set-and-get-value
  (with-fixture state ()
    (setf (externalized-slot-value obj 'foobar) 2)
    (is (eql 2 (externalized-slot-value obj 'foobar)))))

(test save-and-restore-from-transactions
  (tmpdir:with-tmpdir (dir)
    (with-test-store (:dir dir)
      (let ((obj (make-instance 'fake-class)))
        (setf (externalized-slot-value obj 'foobar) 3)
        (Setf (externalized-slot-value obj 'car) 4)))
    (with-test-store (:dir dir)
      (let ((obj (first (bknr.datastore:class-instances 'fake-class))))
        (is (eql 3 (externalized-slot-value obj 'foobar)))
        (is (eql 4 (externalized-slot-value obj 'car)))))))

(test save-and-restore-from-snapshot
  (tmpdir:with-tmpdir (dir)
    (with-test-store (:dir dir)
      (let ((obj (make-instance 'fake-class)))
        (setf (externalized-slot-value obj 'foobar) 3)
        (Setf (externalized-slot-value obj 'car) 4))
      (util:safe-snapshot))
    (with-test-store (:dir dir)
      (let ((obj (first (bknr.datastore:class-instances 'fake-class))))
        (is (eql 3 (externalized-slot-value obj 'foobar)))
        (is (eql 4 (externalized-slot-value obj 'car)))))))


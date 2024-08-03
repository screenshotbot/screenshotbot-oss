;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-simple-object-snapshot
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/store/simple-object-snapshot
                #:simple-object-snapshot)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object
                #:class-instances)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:assert-that))
(in-package :util/store/test-simple-object-snapshot)

(util/fiveam:def-suite)

(def-fixture state (&key dir)
  (with-test-store (:dir dir)
    (&body)))

(defclass some-object (store-object)
  ((slot1 :initarg :slot1)
   (slot2 :initarg :slot2))
  (:metaclass persistent-class))

(defmethod bknr.datastore:make-object-snapshot ((self some-object))
  (make-instance 'simple-object-snapshot
                 :object self
                 :except-slots '(slot2)))

(test happy-snapshot-restore-path
  (tmpdir:with-tmpdir (dir)
    (with-fixture state (:dir dir)
      (make-instance 'some-object
                     :slot1 "foo"
                     :slot2 "bar")
      (bknr.datastore:snapshot))
    (with-fixture state (:dir dir)
      (assert-that (class-instances 'some-object)
                   (has-length 1))
      (let ((obj (car (class-instances 'some-object))))
        (is (equal "foo" (slot-value obj 'slot1)))
        (is (equal "bar" (slot-value obj 'slot2)))))))

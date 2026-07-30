;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-unlikely-to-change-snapshot
  (:use #:cl
        #:fiveam)
  (:import-from #:bknr.datastore
                #:*object-changed-hook*
                #:encode-class-layouts
                #:touched-objects
                #:store-object-subsystem
                #:write-encode-set-slots-in-background
                #:encode-object-slots
                #:encode-set-slots
                #:snapshot-coordinator
                #:encode-set-slots-for-snapshot
                #:make-object-snapshot-v2
                #:class-instances
                #:store-object
                #:persistent-class)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/store/unlikely-to-change-snapshot
                #:object-changed-during-snapshot
                #:unlikely-to-change-snapshot)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length))
(in-package :util/store/test-unlikely-to-change-snapshot)


(util/fiveam:def-suite)

(defclass some-object (store-object)
  ((slot1 :initarg :slot1)
   (slot2 :initarg :slot2))
  (:metaclass persistent-class))

(defmethod bknr.datastore:make-object-snapshot-v2 ((self some-object) now)
  (make-instance 'unlikely-to-change-snapshot
                 :object self))

(def-fixture state (&key dir)
  (with-test-store (:dir dir)
    (unwind-protect
         (&body)
      (setf *object-changed-hook* nil))))

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

(test snapshot-does-not-crash-if-unchanged
  (with-test-store ()
    (uiop:with-temporary-file (:pathname pathname)
      (let* ((obj (make-instance 'some-object))
             (snapshot-coordinator (make-instance 'snapshot-coordinator
                                                  :subsystem (make-instance 'store-object-subsystem)
                                                  :all-objects (list obj)
                                                  :snapshot-pathname pathname)))
        (encode-class-layouts snapshot-coordinator)
        (encode-object-slots snapshot-coordinator)
        (finishes
          (write-encode-set-slots-in-background snapshot-coordinator))))))

(test snapshot-crashes-if-changed
  (with-test-store ()
    (uiop:with-temporary-file (:pathname pathname)
      (let* ((obj (make-instance 'some-object))
             (snapshot-coordinator (make-instance 'snapshot-coordinator
                                                  :subsystem (make-instance 'store-object-subsystem)
                                                  :all-objects (list obj)
                                                  :snapshot-pathname pathname)))
        (encode-class-layouts snapshot-coordinator)        
        (encode-object-slots snapshot-coordinator)
        (setf (slot-value obj 'slot2) :hello)
        (is-true (gethash obj (touched-objects snapshot-coordinator)))
        (signals object-changed-during-snapshot
          (write-encode-set-slots-in-background snapshot-coordinator))
        ;; Technically this shouldn't be part of this test, but whatever
        (is (eql nil *object-changed-hook*))))))


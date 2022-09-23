;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-auto-cleanup
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/auto-cleanup
                #:dispatch-cleanups
                #:register-auto-cleanup
                #:*cleanups*)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.datastore
                #:all-store-objects)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-auto-cleanup)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((*cleanups* nil))
      (register-auto-cleanup
       'simple-obj
       :timestamp #'ts)
      (&body))))

(defclass simple-obj (store-object)
  ((ts :initarg :ts
       :reader ts))
  (:default-initargs :ts (get-universal-time))
  (:metaclass persistent-class))

(defclass simple-obj-2 (store-object)
  ((ts :initarg :ts
       :reader ts))
  (:default-initargs :ts (get-universal-time))
  (:metaclass persistent-class))

(test simple-cleanup
  (with-fixture state ()
    (is (= 1 (length *cleanups*)))
    (register-auto-cleanup
     'simple-obj
     :timestamp #'ts)
    (is (= 1 (length *cleanups*)))))

(test attempt-cleanup
  (with-fixture state ()
    (make-instance 'simple-obj)
    (dispatch-cleanups)
    (is (eql 1 (length (class-instances 'simple-obj))))))

(test attempt-cleanup-with-old-objects
  (with-fixture state ()
    (let ((obj1 (make-instance 'simple-obj))
          (obj2 (make-instance 'simple-obj :ts (- (get-universal-time)
                                                  (* 24 3600 60)))))
      (dispatch-cleanups)
      (is (equal (list obj1)
                 (class-instances 'simple-obj))))))

(test only-delete-the-right-objects
  (with-fixture state ()
    (let ((ts (- (get-universal-time)
                 (* 24 3600 60))))
     (let ((obj1 (make-instance 'simple-obj-2 :ts ts))
           (obj2 (make-instance 'simple-obj :ts ts)))
       (dispatch-cleanups)
       (is (equal (list obj1)
                  (all-store-objects)))))))

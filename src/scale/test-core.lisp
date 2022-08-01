;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :scale/test-core
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:scale/core
                #:delete-instance
                #:delete-stale-instances
                #:%get-universal-time
                #:base-instance)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.datastore
                #:object-destroyed-p)
  (:local-nicknames (#:a #:alexandria)))
(in-package :scale/test-core)


(util/fiveam:def-suite)

(defvar *delete-count* 0)

(def-fixture state ()
  (let ((*delete-count* 0)
        (*start-time* 9000000))
    (cl-mock:with-mocks ()
     (with-test-store ()
       (&body)))))


(defclass fake-instance (base-instance)
  ()
  (:metaclass persistent-class))

(defmethod delete-instance ((self fake-instance))
  (incf *delete-count*))


(test cleanup
  (with-fixture state ()
    (cl-mock:if-called '%get-universal-time
                       (lambda ()
                         *start-time*))
    (let ((instance (make-instance 'fake-instance)))
      (delete-stale-instances)
      (is (eql 0 *delete-count*))
      (is-false (object-destroyed-p instance)))))

(test cleanup-cleans-object
  (with-fixture state ()
    (cl-mock:if-called '%get-universal-time
                       (lambda ()
                         (log:info "old get-universal-time called")
                         *start-time*))
    (let ((instance (make-instance 'fake-instance)))
      (cl-mock:if-called '%get-universal-time
                         (lambda ()
                           (log:info "new get-universal-time called")
                           (+ *start-time* (* 3 3600)))
                         :at-start t)
      (make-instance 'fake-instance)
      (delete-stale-instances)
      (is (eql 1 *delete-count*))
      (is-true (object-destroyed-p instance)))))

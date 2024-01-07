;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/test-view
  (:use #:cl
        #:fiveam)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:cl-mock
                #:if-called))
(in-package :auth/test-view)

(util/fiveam:def-suite)

(defclass my-user ()
  ())

(defmethod auth:can-view ((obj (eql :one)) (user my-user))
  t)

(defmethod auth:can-view (obj (user my-user))
  nil)

(defmethod auth:can-edit ((obj (eql :one)) (user my-user))
  t)

(test happy-path-can-edit-view
  (cl-mock:with-mocks ()
    (let ((user (make-instance 'my-user)))
      (if-called 'auth:current-user
                 (lambda ()
                   user))
      (is-true (auth:can-view :one user))
      (finishes (auth:can-view! :one))
      (finishes (auth:can-edit! :one)))))

(test happy-path-cannot-edit-view
  (cl-mock:with-mocks ()
    (let ((user (make-instance 'my-user)))
      (if-called 'auth:current-user
                 (lambda ()
                   user))
      (is-false (auth:can-view :two user))
      (signals auth:no-access-error
        (auth:can-view! :two))
      (signals auth:no-access-error
        (auth:can-edit! :two)))))

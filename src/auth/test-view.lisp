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
                #:if-called)
  (:import-from #:auth
                #:authenticated-request)
  (:import-from #:auth/viewer-context
                #:normal-viewer-context)
  (:import-from #:auth/request
                #:abstract-authenticated-request))
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
     (let ((hunchentoot:*request*
             (make-instance 'abstract-authenticated-request
                            :viewer-context
                            (make-instance 'normal-viewer-context :user user))))
       (is-true (auth:can-view :one user))
       (finishes (auth:can-view! :one))
       (finishes (auth:can-edit! :one))))))

(test happy-path-cannot-edit-view
  (cl-mock:with-mocks ()
    (let ((user (make-instance 'my-user)))
      (let ((hunchentoot:*request*
              (make-instance 'abstract-authenticated-request
                             :viewer-context
                             (make-instance 'normal-viewer-context :user user))))
        (is-false (auth:can-view :two user))
        (signals auth:no-access-error
          (auth:can-view! :two))
        (signals auth:no-access-error
          (auth:can-edit! :two))))))

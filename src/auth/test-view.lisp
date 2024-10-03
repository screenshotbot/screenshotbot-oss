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
                #:api-viewer-context
                #:anonymous-viewer-context
                #:normal-viewer-context)
  (:import-from #:auth/request
                #:abstract-authenticated-request)
  (:import-from #:util/events
                #:push-event
                #:*events*)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:core/api/model/api-key
                #:api-key)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class))
(in-package :auth/test-view)

(util/fiveam:def-suite)

(defclass my-user (store-object)
  ()
  (:metaclass persistent-class))

(defmethod auth:can-view ((obj (eql :one)) (user my-user))
  t)

(defmethod auth:can-view (obj (user my-user))
  nil)

(defmethod auth:can-edit ((obj (eql :one)) (user my-user))
  t)

(def-fixture state ()
  (with-test-store ()
   (cl-mock:with-mocks ()
     (let ((events))
       (&body)))))

(test happy-path-can-edit-view
  (with-fixture state ()
    (let ((user (make-instance 'my-user)))
     (let ((hunchentoot:*request*
             (make-instance 'abstract-authenticated-request
                            :viewer-context
                            (make-instance 'normal-viewer-context :user user))))
       (is-true (auth:can-view :one user))
       (finishes (auth:can-view! :one))
       (finishes (auth:can-edit! :one))))))

(test happy-path-cannot-edit-view
  (with-fixture state ()
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

(test no-access-error-with-api-viewer-context
  (with-fixture state ()
    (let ((user (make-instance 'my-user)))
      (let ((hunchentoot:*request*
              (make-instance 'abstract-authenticated-request
                             :viewer-context
                             (make-instance 'api-viewer-context
                                            :api-key
                                            (make-instance 'api-key
                                                           :user user)
                                            :user user))))
        (is-false (auth:can-view :two user))
        (signals auth:no-access-error
          (auth:can-view! :two))
        (signals auth:no-access-error
          (auth:can-edit! :two))))))

(defmethod auth:can-viewer-view (vc (obj (eql :one)))
  t)

(test can-view-prioritizes-objects-over-vc
  ;; If the order is reversed, currently this will fail with an error
  (is-true
   (auth:can-viewer-view
    (make-instance 'normal-viewer-context :user :fake-user)
    :one)))


(test cannot-view-null-object
  (is-false (auth:can-viewer-view
             (make-instance 'normal-viewer-context :user :fake-user)
             nil)))

(test event-gets-notified-once
  (with-fixture state ()
    (if-called 'push-event
               (lambda (name &rest args)
                 (push name events)))
    (is-false (auth:can-viewer-view
               (make-instance 'normal-viewer-context :user :fake-user)
               nil))
    (assert-that events
                 (has-length 1))))

(defmethod auth:can-viewer-view (vc (obj (eql :run-1)))
  (auth:can-viewer-view vc :channel-1))

(defmethod auth:can-viewer-view (vc (obj (eql :channel-1)))
  nil)

(test nested-invocation-only-gets-notified-once
  (with-fixture state ()
    (if-called 'push-event
               (lambda (name &rest args)
                 (push name events)))
    (is-false (auth:can-viewer-view
               :vc
               :run-1))
    (assert-that events
                 (has-length 1))))

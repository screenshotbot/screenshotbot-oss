;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/test-request
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:auth
                #:with-sessions)
  (:import-from #:core/installation/installation
                #:abstract-installation
                #:*installation*)
  (:import-from #:core/installation/auth
                #:company-for-request)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:auth/viewer-context
                #:sso-viewer-context
                #:normal-viewer-context))
(in-package :auth/test-request)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-fake-request ()
      (with-sessions ()
        (&body)))))

(test authenticate-request-happy-path
  (with-fixture state ()
   (let ((request (make-instance 'auth:authenticated-request
                                 :uri "/foo"
                                 :headers-in nil)))
     (finishes
       (auth:authenticate-request request)))))

(defclass my-installation (abstract-installation)
  ())

(defmethod company-for-request ((self my-installation) request)
  nil)

(test authenticates-users-from-session
  (with-fixture state ()
    (let ((*installation* (make-instance 'my-installation)))
      (let ((request (make-instance 'auth:authenticated-request
                                    :uri "/foo"
                                    :headers-in nil)))
        (setf (auth:session-value :user) :foobar)
        (auth:authenticate-request request)
        (is (equal :foobar
                   (auth:request-user request)))
        (assert-that
         (auth:viewer-context request)
         (has-typep 'normal-viewer-context))))))

(defmethod auth:can-viewer-view ((vc sso-viewer-context) (obj (eql 'my-comp)))
  t)

(test creates-an-sso-viewer-context-when-needed
  (with-fixture state ()
    (let ((*installation* (make-instance 'my-installation)))
      (let ((request (make-instance 'auth:authenticated-request
                                    :uri "/foo"
                                    :headers-in nil)))
        (setf (auth:session-value :user) 'foobar)
        (setf (auth:session-value :sso-company) 'my-comp)
        (auth:authenticate-request request)
        (is (eql  'foobar
                  (auth:request-user request)))
        (roles:ensure-has-role 'my-comp 'foobar 'roles:standard-member)
        (assert-that
         (auth:viewer-context request)
         (has-typep 'sso-viewer-context))
        (is (eql 'my-comp
                 (auth:request-account request)))))))

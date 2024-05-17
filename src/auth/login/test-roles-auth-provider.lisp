;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/login/test-roles-auth-provider
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:auth/login/roles-auth-provider
                #:roles-auth-provider)
  (:import-from #:core/installation/auth-provider
                #:on-user-sign-in)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:auth/model/roles
                #:user-roles)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:assert-that))
(in-package :auth/login/test-roles-auth-provider)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((*installation* 'fake-installation))
     (let ((user 'test-user))
       (&body)))))

(test simple-on-user-sign-in
  (with-fixture state ()
    (let ((auth-provider (make-instance 'roles-auth-provider)))
      (finishes
        (on-user-sign-in auth-provider user))
      (assert-that
       (bknr.datastore:class-instances 'user-roles)
       (has-length 0)))))

(test simple-on-user-sign-in-with-a-company-provider-that-returns-nil
  (with-fixture state ()
    (let ((auth-provider (make-instance 'roles-auth-provider
                                        :company-provider (lambda () nil))))
      (finishes
        (on-user-sign-in auth-provider user))
      (assert-that
       (bknr.datastore:class-instances 'user-roles)
       (has-length 0)))))

(test simple-on-user-sign-in-with-a-company-provider-that-returns-something
  (with-fixture state ()
    (let ((auth-provider (make-instance 'roles-auth-provider
                                        :company-provider (lambda () 'something))))
      (finishes
        (on-user-sign-in auth-provider user))
      (assert-that
       (bknr.datastore:class-instances 'user-roles)
       (has-length 1))
      (is-true
       (roles:has-role-p 'something user 'roles:standard-member)))))

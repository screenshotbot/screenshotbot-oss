;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-oidc
  (:use :cl)
  (:import-from #:auth
                #:oauth-user-email
                #:oauth-user-user)
  (:import-from #:bknr.datastore
                #:convert-slot-value-while-restoring)
  (:import-from #:cl-mock
                #:if-called)
  (:import-from #:it.bese.fiveam
                #:def-fixture
                #:is
                #:test
                #:with-fixture)
  (:import-from #:oidc/oidc
                #:after-authentication)
  (:import-from #:screenshotbot/login/oidc
                #:%email
                #:%user
                #:oidc-provider
                #:oidc-user)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/login/test-oidc)

(util/fiveam:def-suite)

(def-fixture state (&key expiration-seconds)
  (cl-mock:with-mocks ()
   (with-test-store ()
     (with-fake-request ()
       (with-test-user (:user user)
         (auth:with-sessions ()
           (let ((auth (make-instance 'oidc-provider
                                      :expiration-seconds expiration-seconds
                                      :identifier 'foo)))
             (&body))))))))

(test happy-path
  (with-fixture state ()
    (after-authentication auth
                          :user-id (oid user)
                          :email "arnold@tdrhq.com")))

(test expiration-time-is-set
  (let ((got-expires-in nil))
   (with-fixture state (:expiration-seconds 30)
     (if-called '(setf auth:session-value)
                (lambda (user key &key expires-in)
                  (setf got-expires-in expires-in)))
     (after-authentication auth
                           :user-id (oid user)
                           :email "arnold@tdrhq.com")
     (is (eql 30 got-expires-in)))))


(test email-slot
  "These tests are temporary for a migration, you can delete it once the
%email and email slots are merged."
  (with-fixture state ()
   (let ((self (make-instance 'oidc-user
                              :email "foo")))
     (is (equal "foo" (oauth-user-email self)))
     (setf (oauth-user-email self) "bleh")
     (is (equal "bleh" (oauth-user-email self)))
     (is (equal "bleh" (slot-value self '%email)))
     (fiveam:is-false (slot-boundp self 'screenshotbot/login/oidc::email))
     (convert-slot-value-while-restoring
      self 'email "car")
     (is (equal "car" (oauth-user-email self)))
     (is (equal "ack" (oauth-user-email
                       (make-instance 'oidc-user
                                      :old-email-slot "ack")))))))

(test user-slot
  "These tests are temporary for a migration, you can delete it once the
%user and user slots are merged."
  (with-fixture state ()
   (let ((self (make-instance 'oidc-user
                              :user "foo")))
     (is (equal "foo" (oauth-user-user self)))
     (setf (oauth-user-user self) "bleh")
     (is (equal "bleh" (oauth-user-user self)))
     (is (equal "bleh" (slot-value self '%user)))
     (fiveam:is-false (slot-boundp self 'screenshotbot/login/oidc::user))
     (convert-slot-value-while-restoring
      self 'user "car")
     (is (equal "car" (oauth-user-user self)))
     (is (equal "ack" (oauth-user-user
                       (make-instance 'oidc-user
                                      :old-user-slot "ack")))))))

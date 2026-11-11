;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-default-oidc-provider
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/default-oidc-provider
                #:default-oidc-provider
                #:active-default-oidc-provider
                #:activep
                #:activate-default-oidc-provider
                #:deactivate-default-oidc-provider
                #:oidc-providers-for-company
                #:make-oidc-provider
                #:test-oidc-provider
                #:last-oidc-test-result
                #:oidc-test-result-email
                #:oidc-test-result-error
                #:oidc-test-result-tested-by
                #:oidc-test-signin-link)
  (:import-from #:oidc/oidc
                #:oidc-callback
                #:discover)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:with-test-user)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/store/object-id
                #:oid
                #:find-by-oid)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-default-oidc-provider)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (flet ((make-provider ()
             (make-instance 'default-oidc-provider
                            :issuer "https://example.com/"
                            :client-id "client-id"
                            :client-secret "client-secret"
                            :scope "openid email profile")))
      (&body))))

(test new-providers-are-not-active
  (with-fixture state ()
    (let ((provider (make-provider)))
      (is-false (activep provider))
      (is (eql nil (active-default-oidc-provider))))))

(test can-find-by-oid
  (with-fixture state ()
    (let ((provider (make-provider)))
      (is (eql provider (find-by-oid (oid provider)))))))

(test activate-and-deactivate
  (with-fixture state ()
    (let ((provider (make-provider)))
      (activate-default-oidc-provider provider)
      (is-true (activep provider))
      (is (eql provider (active-default-oidc-provider)))
      (deactivate-default-oidc-provider provider)
      (is-false (activep provider))
      (is (eql nil (active-default-oidc-provider))))))

(test activating-replaces-the-active-provider
  (with-fixture state ()
    (let ((one (make-provider))
          (two (make-provider)))
      (activate-default-oidc-provider one)
      (activate-default-oidc-provider two)
      (is-false (activep one))
      (is-true (activep two))
      (is (eql two (active-default-oidc-provider))))))

(test activating-the-active-provider-again
  (with-fixture state ()
    (let ((provider (make-provider)))
      (activate-default-oidc-provider provider)
      (activate-default-oidc-provider provider)
      (is (eql provider (active-default-oidc-provider))))))

(test providers-for-company
  (with-fixture state ()
    (let* ((company (make-instance 'screenshotbot/model/company:company))
           (other (make-instance 'screenshotbot/model/company:company))
           (provider (make-instance 'default-oidc-provider
                                    :company company
                                    :issuer "https://example.com/")))
      (make-instance 'default-oidc-provider
                     :company other
                     :issuer "https://other.example.com/")
      (is (equal (list provider)
                 (fset:convert 'list (oidc-providers-for-company company)))))))

(def-fixture test-login (&key (user-info '((:sub . "sub-1")
                                           (:email . "admin@example.com")))
                              (get-access-token (lambda (&rest args)
                                                  (declare (ignore args))
                                                  :token)))
  (with-test-store ()
    (cl-mock:with-mocks ()
      (with-installation ()
        (with-fake-request ()
          (auth:with-sessions ()
            (with-test-user (:user user :company company)
              (cl-mock:if-called 'discover
                                 (lambda (oidc)
                                   (declare (ignore oidc))
                                   '((:authorization--endpoint . "https://idp.example.com/authorize")
                                     (:token--endpoint . "https://idp.example.com/token")
                                     (:userinfo--endpoint . "https://idp.example.com/userinfo"))))
              (cl-mock:if-called 'oidc/oidc::oauth-get-access-token
                                 get-access-token)
              (cl-mock:if-called 'oidc/oidc::user-info
                                 (lambda (auth token)
                                   (declare (ignore auth token))
                                   user-info))
              (let* ((provider (make-instance 'default-oidc-provider
                                              :company company
                                              :issuer "https://idp.example.com"
                                              :client-id "client-id"
                                              :client-secret "client-secret"
                                              :scope "openid email profile"))
                     (auth (make-oidc-provider provider
                                               :class 'test-oidc-provider
                                               :stored-provider provider
                                               :tested-by user)))
                (&body)))))))))

(test successful-test-login
  (with-fixture test-login ()
    (signals hex:redirected
      (oidc-callback auth "the-code" "/done"))
    (let ((result (last-oidc-test-result provider)))
      (is (equal "admin@example.com" (oidc-test-result-email result)))
      (is (eql nil (oidc-test-result-error result)))
      (is (eql user (oidc-test-result-tested-by result))))
    ;; A test doesn't log anyone in, or create OIDC users
    (is (eql nil (auth:current-user)))
    (is (eql nil (bknr.datastore:class-instances 'screenshotbot/login/oidc:oidc-user)))))

(test test-login-without-an-email
  (with-fixture test-login (:user-info '((:sub . "sub-1")))
    (signals hex:redirected
      (oidc-callback auth "the-code" "/done"))
    (let ((result (last-oidc-test-result provider)))
      (is (eql nil (oidc-test-result-email result)))
      (is (str:containsp "email" (oidc-test-result-error result))))))

(test test-login-error-from-the-idp
  (with-fixture test-login ()
    (signals hex:redirected
      (oidc-callback auth nil "/done"
                     :error "access_denied"
                     :error-description "User is not assigned"))
    (is (equal "access_denied: User is not assigned"
               (oidc-test-result-error (last-oidc-test-result provider))))))

(test test-login-fails-to-get-a-token
  (with-fixture test-login (:get-access-token (lambda (&rest args)
                                                (declare (ignore args))
                                                (error "oauth error: invalid_client")))
    (signals hex:redirected
      (oidc-callback auth "the-code" "/done"))
    (is (str:containsp "invalid_client"
                       (oidc-test-result-error (last-oidc-test-result provider))))
    (is (eql nil (auth:current-user)))))

(test last-test-result-is-the-newest
  (with-fixture test-login ()
    (signals hex:redirected
      (oidc-callback auth nil "/done" :error "first"))
    (signals hex:redirected
      (oidc-callback auth "the-code" "/done"))
    (is (equal "admin@example.com"
               (oidc-test-result-email (last-oidc-test-result provider))))))

(test test-signin-link-goes-to-the-idp
  (with-fixture test-login ()
    (is (str:starts-with-p "https://idp.example.com/authorize?"
                           (oidc-test-signin-link provider user "/done")))))

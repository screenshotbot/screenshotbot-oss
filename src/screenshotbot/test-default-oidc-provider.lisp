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
                #:%activate-default-oidc-provider
                #:activate-default-oidc-provider
                #:oidc-activation-blocker
                #:oidc-test-result
                #:deactivate-default-oidc-provider
                #:oidc-providers-for-company
                #:make-oidc-provider
                #:test-oidc-provider
                #:last-oidc-test-result
                #:oidc-test-result-email
                #:oidc-test-result-error
                #:oidc-test-result-tested-by
                #:oidc-test-signin-link
                #:edit-default-oidc-provider
                #:oidc-edited-at
                #:issuer
                #:client-id
                #:client-secret)
  (:import-from #:oidc/oidc
                #:oidc-callback
                #:discover)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:with-test-user)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:screenshotbot/model/company
                #:get-singleton-company)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature)
  (:import-from #:core/installation/installation
                #:*installation*)
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
      (%activate-default-oidc-provider provider)
      (is-true (activep provider))
      (is (eql provider (active-default-oidc-provider)))
      (deactivate-default-oidc-provider provider)
      (is-false (activep provider))
      (is (eql nil (active-default-oidc-provider))))))

(test activating-replaces-the-active-provider
  (with-fixture state ()
    (let ((one (make-provider))
          (two (make-provider)))
      (%activate-default-oidc-provider one)
      (%activate-default-oidc-provider two)
      (is-false (activep one))
      (is-true (activep two))
      (is (eql two (active-default-oidc-provider))))))

(test activating-the-active-provider-again
  (with-fixture state ()
    (let ((provider (make-provider)))
      (%activate-default-oidc-provider provider)
      (%activate-default-oidc-provider provider)
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

(test editing-a-provider
  (with-fixture state ()
    (let ((provider (make-provider)))
      (is (eql nil (oidc-edited-at provider)))
      (edit-default-oidc-provider provider
                                  :issuer "https://new.example.com/"
                                  :client-id "new-client-id"
                                  :client-secret "new-secret")
      (is (equal "https://new.example.com/" (issuer provider)))
      (is (equal "new-client-id" (client-id provider)))
      (is (equal "new-secret" (client-secret provider)))
      (is-true (oidc-edited-at provider)))))

(test editing-without-a-secret-keeps-the-secret
  (with-fixture state ()
    (let ((provider (make-provider)))
      (edit-default-oidc-provider provider
                                  :issuer "https://new.example.com/"
                                  :client-id "client-id"
                                  :client-secret nil)
      (is (equal "client-secret" (client-secret provider))))))

(test editing-without-changes-isnt-an-edit
  (with-fixture state ()
    (let ((provider (make-provider)))
      (edit-default-oidc-provider provider
                                  :issuer "https://example.com/"
                                  :client-id "client-id"
                                  :client-secret nil)
      (is (eql nil (oidc-edited-at provider))))))

(test cant-change-the-active-provider
  (with-fixture state ()
    (let ((provider (make-provider)))
      (%activate-default-oidc-provider provider)
      (signals error
        (edit-default-oidc-provider provider
                                    :issuer "https://new.example.com/"
                                    :client-id "client-id"
                                    :client-secret nil))
      (signals error
        (edit-default-oidc-provider provider
                                    :issuer "https://example.com/"
                                    :client-id "client-id"
                                    :client-secret "new-secret"))
      (is (equal "https://example.com/" (issuer provider)))
      (is (equal "client-secret" (client-secret provider))))))

(def-fixture activation ()
  (with-test-store ()
    (with-installation ()
      (let* ((company (get-singleton-company *installation*))
             (admin (make-user :email "admin@example.com"))
             (provider (make-instance 'default-oidc-provider
                                      :company company
                                      :issuer "https://example.com/"
                                      :client-id "client-id"
                                      :client-secret "client-secret")))
        (setf (roles:user-role company admin) 'roles:admin)
        (flet ((record-test (&key (email "admin@example.com")
                               error-message
                               (tested-by admin)
                               (ts (get-universal-time)))
                 (make-instance 'oidc-test-result
                                :provider provider
                                :tested-by tested-by
                                :email (unless error-message email)
                                :error-message error-message
                                :ts ts)))
          (declare (ignorable #'record-test))
          (&body))))))

(test activation-requires-a-test
  (with-fixture activation ()
    (is (equal "Test this provider before activating it"
               (oidc-activation-blocker provider admin)))
    (record-test :error-message "access_denied")
    (is (equal "The last test failed"
               (oidc-activation-blocker provider admin)))))

(test activation-requires-own-recent-test
  (with-fixture activation ()
    (let ((other-admin (make-user :email "other@example.com")))
      (setf (roles:user-role company other-admin) 'roles:admin)
      (record-test :tested-by other-admin)
      (is (equal "Test this provider yourself before activating it"
                 (oidc-activation-blocker provider admin))))
    (record-test :ts (- (get-universal-time) (* 2 3600)))
    (is (str:containsp "more than an hour old"
                       (oidc-activation-blocker provider admin)))
    (record-test)
    (is (eql nil (oidc-activation-blocker provider admin)))))

(test activation-doesnt-require-the-test-to-sign-in-as-an-admin
  (with-fixture activation ()
    ;; If nobody can administer the installation after activating, an
    ;; admin can be restored with a recovery token.
    (make-user :email "member@example.com")
    (record-test :email "member@example.com")
    (is (eql nil (oidc-activation-blocker provider admin)))
    (record-test :email "new@example.com")
    (is (eql nil (oidc-activation-blocker provider admin)))))

(test editing-invalidates-tests
  (with-fixture activation ()
    (record-test :ts (- (get-universal-time) 10))
    (is (eql nil (oidc-activation-blocker provider admin)))
    (edit-default-oidc-provider provider
                                :issuer "https://new.example.com/"
                                :client-id "client-id"
                                :client-secret nil)
    (is (equal "The provider changed since the last test, test it again"
               (oidc-activation-blocker provider admin)))
    ;; A test in the same second as the edit doesn't count either
    (record-test)
    (is (equal "The provider changed since the last test, test it again"
               (oidc-activation-blocker provider admin)))))

(test activate-replaces-the-active-provider
  (with-fixture activation ()
    (let ((old (make-instance 'default-oidc-provider
                              :company company
                              :issuer "https://old.example.com/")))
      (%activate-default-oidc-provider old)
      (record-test)
      (activate-default-oidc-provider provider admin)
      (is (eql provider (active-default-oidc-provider)))
      (is-false (activep old))
      (is (equal "This provider is already active"
                 (oidc-activation-blocker provider admin))))))

(test activate-refuses-when-blocked
  (with-fixture activation ()
    (let ((old (make-instance 'default-oidc-provider
                              :company company
                              :issuer "https://old.example.com/")))
      (%activate-default-oidc-provider old)
      (signals simple-error
        (activate-default-oidc-provider provider admin))
      (is (eql old (active-default-oidc-provider)))
      (is-false (activep provider)))))

(test activation-is-only-for-the-singleton-company
  (with-fixture activation ()
    (let ((provider (make-instance 'default-oidc-provider
                                   :company (make-instance 'screenshotbot/model/company:company)
                                   :issuer "https://example.com/")))
      (is (equal "This provider doesn't belong to this installation's company"
                 (oidc-activation-blocker provider admin))))))

(defclass multi-org-installation (multi-org-feature
                                  installation)
  ())

(test activation-is-only-for-single-company-installs
  (with-test-store ()
    (let ((*installation* (make-instance 'multi-org-installation)))
      (let* ((company (make-instance 'screenshotbot/model/company:company))
             (admin (make-user :email "admin@example.com"))
             (provider (make-instance 'default-oidc-provider
                                      :company company
                                      :issuer "https://example.com/")))
        (setf (roles:user-role company admin) 'roles:admin)
        (is (str:containsp "single-company"
                           (oidc-activation-blocker provider admin)))))))

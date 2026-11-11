;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/saml/test-settings
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/saml/settings
                #:company-sso-page
                #:sso-page-url)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:screenshotbot/saml/settings
                #:redeem-recovery-token
                #:session-recovery-token
                #:sso-principal)
  (:import-from #:screenshotbot/login/recovery-token
                #:make-recovery-token)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:screenshotbot/left-side-bar
                #:left-side-bar)
  (:import-from #:auth/viewer-context
                #:anonymous-viewer-context
                #:normal-viewer-context
                #:recovery-viewer-context
                #:viewer-context-company)
  (:import-from #:screenshotbot/saml/settings
                #:submit-sso-form
                #:edit-sso-form
                #:%delete
                #:confirm-delete
                #:confirm-deactivate
                #:sso-form
                #:sp-metadata-url)
  (:import-from #:screenshotbot/login/saml
                #:entity-id
                #:callback-url
                #:saml-auth-provider
                #:saml-name
                #:saml-idp-metadata-url
                #:fetch-metadata-xml)
  (:import-from #:screenshotbot/saml/settings
                #:oidc-form
                #:submit-oidc-form
                #:start-oidc-test-login
                #:edit-oidc-form
                #:%activate-oidc
                #:%activate)
  (:import-from #:screenshotbot/default-oidc-provider
                #:default-oidc-provider
                #:activep
                #:active-default-oidc-provider)
  (:import-from #:oidc/oidc
                #:discover))
(in-package :screenshotbot/saml/test-settings)

(util/fiveam:def-suite)

(def-fixture state (&key (role 'roles:admin) (enabled t))
  (with-test-store ()
    (with-test-user (:user user :company company :logged-in-p t)
      (setf (roles:user-role company user) role)
      (gk:create :self-service-saml)
      (when enabled
        (gk:allow :self-service-saml company))
      (&body))))

(test non-admin-cannot-see-the-page
  (with-fixture state (:role 'roles:standard-member)
    (signals error
      (company-sso-page :oid (oid company)))))

(test needs-the-gatekeeper
  (with-fixture state (:enabled nil)
    (signals error
      (company-sso-page :oid (oid company)))))

(test cannot-see-another-companys-page
  (with-fixture state ()
    (let ((other (make-instance 'company)))
      (gk:allow :self-service-saml other)
      ;; The handler turns the NO-ACCESS-ERROR into a page
      (let ((html (markup:write-html
                   (company-sso-page :oid (oid other)))))
        (is (str:containsp "You do not have permission" html))
        (is (not (str:containsp "Configure Single-Sign-on" html)))))))

(test admin-sees-their-page
  (with-fixture state ()
    (let ((html (markup:write-html
                 (company-sso-page :oid (oid company)))))
      (is (str:containsp "Configure Single-Sign-on" html)))))

(test page-url
  (with-fixture state ()
    (is (equal (format nil "/company/~a/sso" (oid company))
               (sso-page-url company)))))

(def-fixture anonymous ()
  (with-test-store ()
    (with-installation ()
      (with-fake-request ()
        (auth:with-sessions ()
          (let ((company (make-instance 'company)))
            (gk:create :self-service-saml)
            (gk:allow :self-service-saml company)
            (&body)))))))

(defun render-sso-page (company)
  "The rendered SSO page, or NIL if we got something else, e.g. a login
page or a redirect."
  (let ((res (catch 'hunchentoot::handler-done
               (company-sso-page :oid (oid company)))))
    (when res
      (let ((html (markup:write-html res)))
        (when (str:containsp "Configure Single-Sign-on" html)
          html)))))

(test redeeming-a-token-gives-access-to-the-sso-page
  (with-fixture anonymous ()
    (is (eql nil (render-sso-page company)))
    (let ((token (nth-value 1 (make-recovery-token company))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (is (eql nil (auth:current-user)))
      (is (not (eql nil (render-sso-page company))))
      (is (typep (sso-principal company)
                 'screenshotbot/login/recovery-token:recovery-token)))))

(test invalid-token-gives-no-access
  (with-fixture anonymous ()
    (let ((html (markup:write-html (redeem-recovery-token "not-a-token"))))
      (is (str:containsp "invalid or has expired" html)))
    (is (eql nil (session-recovery-token company)))
    (is (eql nil (render-sso-page company)))))

(test token-is-only-for-its-company
  (with-fixture anonymous ()
    (let ((other (make-instance 'company))
          (token (nth-value 1 (make-recovery-token company))))
      (gk:allow :self-service-saml other)
      (signals hex:redirected
        (redeem-recovery-token token))
      (is (eql nil (session-recovery-token other)))
      (is (eql nil (sso-principal other)))
      (is (eql nil (render-sso-page other))))))

(test expired-token-gives-no-access
  (with-fixture anonymous ()
    (multiple-value-bind (recovery-token token)
        (make-recovery-token company)
      (signals hex:redirected
        (redeem-recovery-token token))
      (setf (slot-value recovery-token
                        'screenshotbot/login/recovery-token::expires-at)
            (- (get-universal-time) 1))
      (is (eql nil (session-recovery-token company)))
      (is (eql nil (render-sso-page company))))))

(test token-still-needs-the-gatekeeper
  (with-fixture anonymous ()
    (gk:deny :self-service-saml company)
    (let ((token (nth-value 1 (make-recovery-token company))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (signals error
        (company-sso-page :oid (oid company))))))

(test redeeming-a-token-uses-a-new-session
  (with-fixture anonymous ()
    (let ((old-token (auth::%session-token (auth:current-session)))
          (token (nth-value 1 (make-recovery-token company))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (is (not (equal old-token (auth::%session-token (auth:current-session)))))
      (is (eql nil (auth:session-value :recovery-token
                                       :session (make-instance 'auth::user-session-transient
                                                               :token old-token))))
      (is (not (eql nil (session-recovery-token company)))))))

(test session-access-is-short
  (with-fixture anonymous ()
    (let ((token (nth-value 1 (make-recovery-token company :expires-in (* 24 3600)))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (multiple-value-bind (recovery-token deadline)
          (session-recovery-token company)
        (is (not (eql nil recovery-token)))
        (is (<= deadline (+ (get-universal-time) 3600))))
      ;; The session's access has run out, though the token hasn't
      (destructuring-bind (recovery-token deadline)
          (auth:session-value :recovery-token)
        (declare (ignore deadline))
        (setf (auth:session-value :recovery-token)
              (list recovery-token (- (get-universal-time) 1))))
      (is (eql nil (session-recovery-token company)))
      (is (eql nil (render-sso-page company)))
      ;; Redeeming again gives another hour
      (signals hex:redirected
        (redeem-recovery-token token))
      (is (not (eql nil (session-recovery-token company)))))))

(test session-access-ends-with-the-token
  (with-fixture anonymous ()
    (let ((token (nth-value 1 (make-recovery-token company :expires-in 600))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (is (<= (nth-value 1 (session-recovery-token company))
              (+ (get-universal-time) 600))))))

(defun authenticate ()
  (auth:authenticate-request hunchentoot:*request*)
  (auth:viewer-context hunchentoot:*request*))

(test redeeming-a-token-gives-a-recovery-viewer-context
  (with-fixture anonymous ()
    (is (typep (authenticate) 'anonymous-viewer-context))
    (let ((token (nth-value 1 (make-recovery-token company))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (let ((vc (authenticate)))
        (is (typep vc 'recovery-viewer-context))
        (is (eql company (viewer-context-company vc)))
        (is (eql nil (auth:current-user)))))))

(test expired-recovery-session-is-anonymous
  (with-fixture anonymous ()
    (let ((token (nth-value 1 (make-recovery-token company))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (destructuring-bind (recovery-token deadline)
          (auth:session-value :recovery-token)
        (declare (ignore deadline))
        (setf (auth:session-value :recovery-token)
              (list recovery-token (- (get-universal-time) 1))))
      (is (typep (authenticate) 'anonymous-viewer-context)))))

(test logged-in-user-keeps-their-viewer-context
  (with-fixture anonymous ()
    (let ((token (nth-value 1 (make-recovery-token company))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (let ((vc (make-instance 'normal-viewer-context :user :someone)))
        (setf (auth:request-user hunchentoot:*request*) :someone)
        (setf (auth:viewer-context hunchentoot:*request*) vc)
        (is (eql vc (authenticate)))))))

(test recovery-menu-only-has-the-team-and-sso-pages
  (with-fixture anonymous ()
    (let ((token (nth-value 1 (make-recovery-token company))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (authenticate)
      (let ((html (markup:write-html
                   (left-side-bar :script-name (sso-page-url company)))))
        (is (str:containsp (sso-page-url company) html))
        (is (str:containsp "href=\"/team\"" html))
        (is (str:containsp "using a recovery token" html))
        (is (not (str:containsp "Recent Runs" html)))
        (is (not (str:containsp "API Keys" html)))))))

(def-fixture edit ()
  (cl-mock:with-mocks ()
    (cl-mock:if-called 'fetch-metadata-xml
                       (lambda (url)
                         (declare (ignore url))
                         "<xml />"))
    (let ((saml (make-instance 'saml-auth-provider
                               :company company
                               :name "Okta"
                               :idp-metadata-url "https://old.example.com/metadata")))
      (&body))))

(test edit-form-is-prefilled
  (with-fixture state ()
    (with-fixture edit ()
      (let ((html (markup:write-html (edit-sso-form saml))))
        (is (str:containsp "Edit SAML connection" html))
        (is (str:containsp "https://old.example.com/metadata" html))
        (is (str:containsp "Okta" html))))))

(test submitting-the-edit-form
  (with-fixture state ()
    (with-fixture edit ()
      (signals hex:redirected
        (submit-sso-form company :saml saml
                                 :name "Entra"
                                 :idp-metadata "https://new.example.com/metadata"))
      (is (equal "Entra" (saml-name saml)))
      (is (equal "https://new.example.com/metadata" (saml-idp-metadata-url saml))))))

(test cant-change-the-idp-of-the-active-provider-in-the-form
  (with-fixture state ()
    (with-fixture edit ()
      (setf (slot-value saml 'screenshotbot/login/saml::%defaultp) t)
      (let ((html (markup:write-html
                   (submit-sso-form company :saml saml
                                            :name "Okta"
                                            :idp-metadata "https://new.example.com/metadata"))))
        (is (str:containsp "invalid-feedback" html)))
      (is (equal "https://old.example.com/metadata" (saml-idp-metadata-url saml))))))

(test cant-edit-another-companys-provider
  (with-fixture state ()
    (with-fixture edit ()
      (let ((other (make-instance 'company)))
        (gk:allow :self-service-saml other)
        (signals error
          (submit-sso-form other :saml saml
                                 :name "x"
                                 :idp-metadata "https://new.example.com/metadata"))
        (is (equal "Okta" (saml-name saml)))))))

(test non-admin-cant-edit
  (with-fixture state (:role 'roles:standard-member)
    (with-fixture edit ()
      (signals error
        (edit-sso-form saml))
      (signals error
        (submit-sso-form company :saml saml
                                 :name "x"
                                 :idp-metadata "https://new.example.com/metadata"))
      (is (equal "Okta" (saml-name saml))))))

(test delete-from-the-page
  (with-fixture state ()
    (with-fixture edit ()
      (signals hex:redirected
        (%delete saml company))
      (is-true (bknr.indices:object-destroyed-p saml))
      ;; A second click is fine
      (signals hex:redirected
        (%delete saml company)))))

(test non-admin-cant-delete
  (with-fixture state (:role 'roles:standard-member)
    (with-fixture edit ()
      (signals error
        (%delete saml company))
      (is-false (bknr.indices:object-destroyed-p saml)))))

(test cant-delete-through-another-company
  (with-fixture state ()
    (with-fixture edit ()
      (let ((other (make-instance 'company)))
        (gk:allow :self-service-saml other)
        (setf (roles:user-role other user) 'roles:admin)
        (signals error
          (%delete saml other))
        (is-false (bknr.indices:object-destroyed-p saml))))))

(test delete-asks-for-confirmation
  (with-fixture state ()
    (with-fixture edit ()
      (let ((html (markup:write-html (confirm-delete saml company))))
        (is (str:containsp "Delete the SAML connection" html))
        (is (str:containsp "Okta" html)))
      (is-false (bknr.indices:object-destroyed-p saml)))))

(test deactivate-asks-for-confirmation
  (with-fixture state ()
    (with-fixture edit ()
      (setf (slot-value saml 'screenshotbot/login/saml::%defaultp) t)
      (let ((html (markup:write-html (confirm-deactivate saml))))
        (is (str:containsp "locked out" html)))
      (is-true (screenshotbot/login/saml:defaultp saml)))))

(test non-admin-cant-see-the-confirmations
  (with-fixture state (:role 'roles:standard-member)
    (with-fixture edit ()
      (signals error
        (confirm-delete saml company))
      (signals error
        (confirm-deactivate saml)))))

(test form-shows-the-idp-settings
  (with-fixture state ()
    (let ((html (markup:write-html (sso-form company))))
      (is (str:containsp (entity-id) html))
      (is (str:containsp (callback-url) html))
      ;; The per-company metadata URL is not our entity ID, and on a
      ;; single-company install we don't show it at all
      (is (not (str:containsp "/saml/entity/" html))))))

(defclass multi-org-installation (screenshotbot/installation:multi-org-feature
                                  screenshotbot/installation:installation)
  ())

(test sp-metadata-url-on-multi-org
  (with-fixture state ()
    (is (equal (entity-id) (sp-metadata-url company)))
    (let ((*installation* (make-instance 'multi-org-installation)))
      (is (str:containsp (format nil "/saml/entity/~a/metadata" (oid company))
                         (sp-metadata-url company))))))

(def-fixture oidc (&key (discover (lambda (oidc)
                                     (declare (ignore oidc))
                                     '((:issuer . "https://idp.example.com")))))
  (cl-mock:with-mocks ()
    (cl-mock:if-called 'discover discover)
    (&body)))

(defun oidc-providers ()
  (bknr.datastore:class-instances 'default-oidc-provider))

(test page-has-a-create-oidc-button
  (with-fixture state ()
    (let ((html (markup:write-html
                 (company-sso-page :oid (oid company)))))
      (is (str:containsp "Create OIDC" html)))))

(test oidc-form-renders
  (with-fixture state ()
    (with-fixture oidc ()
      (let ((html (markup:write-html (oidc-form company))))
        (is (str:containsp "New OIDC connection" html))
        (is (str:containsp "/account/oauth-callback" html))))))

(test creating-an-oidc-provider
  (with-fixture state ()
    (with-fixture oidc ()
      (signals hex:redirected
        (submit-oidc-form company
                          :issuer "https://idp.example.com"
                          :client-id "client-id"
                          :client-secret "client-secret"))
      (let ((provider (first (oidc-providers))))
        (is (eql company (screenshotbot/default-oidc-provider::company provider)))
        (is (equal "https://idp.example.com"
                   (screenshotbot/default-oidc-provider::issuer provider)))
        (is (equal "client-secret"
                   (screenshotbot/default-oidc-provider::client-secret provider)))
        (is (equal "openid email profile"
                   (screenshotbot/default-oidc-provider::scope provider)))
        ;; Creating it must not change anyone's login
        (is-false (activep provider))
        (is (eql nil (active-default-oidc-provider)))))))

(test oidc-form-validation
  (with-fixture state ()
    (with-fixture oidc ()
      (dolist (args '((:issuer "" :client-id "a" :client-secret "b")
                      (:issuer "http://idp.example.com" :client-id "a" :client-secret "b")
                      (:issuer "https://idp.example.com" :client-id "" :client-secret "b")
                      (:issuer "https://idp.example.com" :client-id "a" :client-secret "")))
        (multiple-value-bind (html errors)
            (apply #'submit-oidc-form company args)
          (declare (ignore html))
          (is-true errors)))
      (is (eql nil (oidc-providers))))))

(test oidc-form-checks-discovery
  (with-fixture state ()
    (with-fixture oidc (:discover (lambda (oidc)
                                    (declare (ignore oidc))
                                    (error "can't connect")))
      (multiple-value-bind (html errors)
          (submit-oidc-form company
                            :issuer "https://idp.example.com"
                            :client-id "client-id"
                            :client-secret "client-secret")
        (declare (ignore html))
        (is (assoc :issuer errors)))
      (is (eql nil (oidc-providers))))))

(test oidc-errors-dont-send-the-secret-back
  (with-fixture state ()
    (with-fixture oidc ()
      (let ((html (markup:write-html
                   (submit-oidc-form company
                                     :issuer ""
                                     :client-id "client-id"
                                     :client-secret "very-secret"))))
        (is (not (str:containsp "very-secret" html)))))))

(test non-admin-cant-create-oidc
  (with-fixture state (:role 'roles:standard-member)
    (with-fixture oidc ()
      (signals error
        (oidc-form company))
      (signals error
        (submit-oidc-form company
                          :issuer "https://idp.example.com"
                          :client-id "client-id"
                          :client-secret "client-secret"))
      (is (eql nil (oidc-providers))))))

(defun make-oidc-provider (company &key (issuer "https://idp.example.com")
                                         (client-secret "client-secret"))
  (make-instance 'default-oidc-provider
                 :company company
                 :issuer issuer
                 :client-id "client-id"
                 :client-secret client-secret
                 :scope "openid email profile"))

(test page-lists-oidc-providers
  (with-fixture state ()
    (make-oidc-provider company :issuer "https://one.example.com")
    (make-oidc-provider company :issuer "https://two.example.com")
    (let ((html (markup:write-html
                 (company-sso-page :oid (oid company)))))
      (is (str:containsp "https://one.example.com" html))
      (is (str:containsp "https://two.example.com" html))
      (is (str:containsp "Test this provider before activating it" html))
      (is (not (str:containsp "client-secret" html))))))

(test page-shows-the-active-oidc-provider
  (with-fixture state ()
    (let ((provider (make-oidc-provider company)))
      (screenshotbot/default-oidc-provider::%activate-default-oidc-provider provider)
      (let ((html (markup:write-html
                   (company-sso-page :oid (oid company)))))
        (is (str:containsp "Active" html))
        (is (not (str:containsp "Inactive" html)))))))

(test page-doesnt-list-another-companys-oidc-providers
  (with-fixture state ()
    (make-oidc-provider (make-instance 'company) :issuer "https://other.example.com")
    (let ((html (markup:write-html
                 (company-sso-page :oid (oid company)))))
      (is (not (str:containsp "https://other.example.com" html))))))

(defun make-oidc-test-result (provider user &key email error-message)
  (make-instance 'screenshotbot/default-oidc-provider::oidc-test-result
                 :provider provider
                 :tested-by user
                 :email email
                 :error-message error-message))

(test oidc-rows-have-a-test-button
  (with-fixture state ()
    (make-oidc-provider company)
    (let ((html (markup:write-html
                 (company-sso-page :oid (oid company)))))
      (is (str:containsp ">Test</a>" html))
      (is (str:containsp "Never tested" html)))))

(test oidc-rows-show-the-last-test-result
  (with-fixture state ()
    (let ((provider (make-oidc-provider company)))
      (make-oidc-test-result provider user :error-message "access_denied: nope")
      (make-oidc-test-result provider user :email "admin@example.com")
      (let ((html (markup:write-html
                   (company-sso-page :oid (oid company)))))
        (is (str:containsp "Signed in as admin@example.com" html))
        (is (not (str:containsp "access_denied" html)))))))

(test oidc-rows-show-a-failed-test
  (with-fixture state ()
    (let ((provider (make-oidc-provider company)))
      (make-oidc-test-result provider user :error-message "access_denied: nope")
      (let ((html (markup:write-html
                   (company-sso-page :oid (oid company)))))
        (is (str:containsp "Failed: access_denied: nope" html))))))

(test starting-an-oidc-test-login
  (with-fixture state ()
    (with-fixture oidc (:discover (lambda (oidc)
                                    (declare (ignore oidc))
                                    '((:authorization--endpoint . "https://idp.example.com/authorize"))))
      (let ((provider (make-oidc-provider company)))
        (handler-case
            (progn
              (start-oidc-test-login provider)
              (fail "expected a redirect"))
          (hex:redirected (e)
            (is (str:starts-with-p "https://idp.example.com/authorize?"
                                   (slot-value e 'hex::url)))))))))

(test non-admin-cant-start-an-oidc-test-login
  (with-fixture state (:role 'roles:standard-member)
    (with-fixture oidc ()
      (let ((provider (make-oidc-provider company)))
        (signals error
          (start-oidc-test-login provider))))))

(test oidc-edit-form-is-prefilled
  (with-fixture state ()
    (with-fixture oidc ()
      (let* ((provider (make-oidc-provider company :issuer "https://old.example.com"
                                                   :client-secret "very-secret"))
             (html (markup:write-html (edit-oidc-form provider))))
        (is (str:containsp "Edit OIDC connection" html))
        (is (str:containsp "https://old.example.com" html))
        (is (str:containsp "value=\"client-id\"" html))
        ;; The secret is never sent to the browser
        (is (not (str:containsp "very-secret" html)))))))

(test submitting-the-oidc-edit-form
  (with-fixture state ()
    (with-fixture oidc ()
      (let ((provider (make-oidc-provider company)))
        (signals hex:redirected
          (submit-oidc-form company :oidc provider
                                    :issuer "https://new.example.com"
                                    :client-id "new-client-id"
                                    :client-secret ""))
        (is (equal "https://new.example.com"
                   (screenshotbot/default-oidc-provider::issuer provider)))
        (is (equal "new-client-id"
                   (screenshotbot/default-oidc-provider::client-id provider)))
        ;; Blank keeps the old secret
        (is (equal "client-secret"
                   (screenshotbot/default-oidc-provider::client-secret provider)))
        ;; Editing doesn't create a new provider
        (is (equal (list provider) (oidc-providers)))))))

(test oidc-edit-form-validation
  (with-fixture state ()
    (with-fixture oidc ()
      (let ((provider (make-oidc-provider company :issuer "https://old.example.com")))
        (multiple-value-bind (html errors)
            (submit-oidc-form company :oidc provider
                                      :issuer "http://new.example.com"
                                      :client-id "client-id"
                                      :client-secret "")
          (declare (ignore html))
          (is (assoc :issuer errors)))
        (is (equal "https://old.example.com"
                   (screenshotbot/default-oidc-provider::issuer provider)))))))

(test cant-edit-the-active-oidc-provider
  (with-fixture state ()
    (with-fixture oidc ()
      (let ((provider (make-oidc-provider company :issuer "https://old.example.com")))
        (screenshotbot/default-oidc-provider::%activate-default-oidc-provider provider)
        (let ((html (markup:write-html
                     (company-sso-page :oid (oid company)))))
          (is (str:containsp "it can&#39;t be changed" html)))
        (let ((html (markup:write-html
                     (submit-oidc-form company :oidc provider
                                               :issuer "https://new.example.com"
                                               :client-id "client-id"
                                               :client-secret ""))))
          (is (str:containsp "invalid-feedback\">This connection is active" html)))
        (is (equal "https://old.example.com"
                   (screenshotbot/default-oidc-provider::issuer provider)))))))

(test cant-edit-another-companys-oidc-provider
  (with-fixture state ()
    (with-fixture oidc ()
      (let* ((other (make-instance 'company))
             (provider (make-oidc-provider company :issuer "https://old.example.com")))
        (gk:allow :self-service-saml other)
        (signals error
          (submit-oidc-form other :oidc provider
                                  :issuer "https://new.example.com"
                                  :client-id "client-id"
                                  :client-secret ""))
        (is (equal "https://old.example.com"
                   (screenshotbot/default-oidc-provider::issuer provider)))))))

(test non-admin-cant-edit-oidc
  (with-fixture state (:role 'roles:standard-member)
    (with-fixture oidc ()
      (let ((provider (make-oidc-provider company :issuer "https://old.example.com")))
        (signals error
          (edit-oidc-form provider))
        (signals error
          (submit-oidc-form company :oidc provider
                                    :issuer "https://new.example.com"
                                    :client-id "client-id"
                                    :client-secret ""))
        (is (equal "https://old.example.com"
                   (screenshotbot/default-oidc-provider::issuer provider)))))))

(test oidc-edit-errors-dont-send-the-secret-back
  (with-fixture state ()
    (with-fixture oidc ()
      (let* ((provider (make-oidc-provider company :client-secret "stored-secret"))
             (html (markup:write-html
                    (submit-oidc-form company :oidc provider
                                              :issuer ""
                                              :client-id "client-id"
                                              :client-secret "typed-secret"))))
        (is (str:containsp "invalid-feedback" html))
        (is (not (str:containsp "stored-secret" html)))
        (is (not (str:containsp "typed-secret" html)))))))

(defun make-passing-oidc-test (provider user)
  (setf (auth:user-email user) "admin@example.com")
  (make-oidc-test-result provider user :email "admin@example.com"))

(test oidc-activate-button-is-disabled-without-a-test
  (with-fixture state ()
    (make-oidc-provider company)
    (let ((html (markup:write-html
                 (company-sso-page :oid (oid company)))))
      (is (str:containsp "Test this provider before activating it" html))
      (is (str:containsp "disabled=\"disabled\">Activate" html)))))

(test oidc-activate-button-after-a-passing-test
  (with-fixture state ()
    (let ((provider (make-oidc-provider company)))
      (make-passing-oidc-test provider user)
      (let ((html (markup:write-html
                   (company-sso-page :oid (oid company)))))
        (is (str:containsp "Ready to activate" html))
        (is (not (str:containsp "disabled=\"disabled\">Activate" html)))))))

(test activating-an-oidc-provider
  (with-fixture state ()
    (let ((provider (make-oidc-provider company)))
      (make-passing-oidc-test provider user)
      (signals hex:redirected
        (%activate-oidc provider))
      (is (eql provider (active-default-oidc-provider))))))

(test activating-oidc-deactivates-saml
  (with-fixture state ()
    (with-fixture edit ()
      (setf (slot-value saml 'screenshotbot/login/saml::%defaultp) t)
      (let ((provider (make-oidc-provider company)))
        (make-passing-oidc-test provider user)
        (signals hex:redirected
          (%activate-oidc provider))
        (is (eql provider (active-default-oidc-provider)))
        (is (eql nil (screenshotbot/login/saml::default-saml-auth-provider)))))))

(test activating-saml-deactivates-oidc
  (with-fixture state ()
    (with-fixture edit ()
      (let ((provider (make-oidc-provider company)))
        (screenshotbot/default-oidc-provider::%activate-default-oidc-provider provider)
        (setf (auth:user-email user) "admin@example.com")
        (make-instance 'screenshotbot/login/saml::saml-test-result
                       :provider saml
                       :tested-by user
                       :email "admin@example.com")
        (signals hex:redirected
          (%activate saml))
        (is (eql saml (screenshotbot/login/saml::default-saml-auth-provider)))
        (is (eql nil (active-default-oidc-provider)))
        (is-false (activep provider))))))

(test cant-activate-oidc-without-a-test
  (with-fixture state ()
    (let ((provider (make-oidc-provider company)))
      (signals error
        (%activate-oidc provider))
      (is (eql nil (active-default-oidc-provider))))))

(test non-admin-cant-activate-oidc
  (with-fixture state (:role 'roles:standard-member)
    (let ((provider (make-oidc-provider company)))
      (make-passing-oidc-test provider user)
      (signals error
        (%activate-oidc provider))
      (is (eql nil (active-default-oidc-provider))))))

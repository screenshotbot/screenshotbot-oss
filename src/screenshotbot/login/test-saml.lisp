;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-saml
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/login/saml
                #:valid-relay-state-p
                #:find-saml-auth-provider-for-response
                #:clear-relay-states
                #:relay-state-expired
                #:relay-state-for-id
                #:relay-state-id
                #:make-relay-state
                #:default-saml-auth-provider
                #:find-or-create-saml-cert
                #:make-cert-pair
                #:find-user-for-sso
                #:saml-auth-provider
                #:authn-request-get-encoded-authn-request
                #:make-authn-request
                #:create-settings-builder-for-xml
                #:parse-xml
                #:sp-metadata
                #:singleton-sp-metadata
                #:handle-saml-response
                #:finish-test-login
                #:make-saml-response
                #:last-saml-test-result
                #:saml-test-result-email
                #:saml-test-result-error
                #:saml-test-result-tested-by
                #:relay-state-settings
                #:activation-blocker
                #:edit-saml-provider
                #:delete-saml-provider
                #:saml-auth-providers-for-company
                #:saml-test-results-for-provider
                #:saml-edited-at
                #:saml-idp-metadata-url
                #:saml-name
                #:activate-saml-provider
                #:deactivate-saml-provider
                #:defaultp
                #:saml-test-result
                #:entity-id
                #:saml-public-cert)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:screenshotbot/model/company
                #:get-singleton-company
                #:company)
  (:import-from #:core/installation/installation
                #:installation-domain
                #:*installation*)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:has-typep)
  (:import-from #:fiveam-matchers/misc
                #:is-not-null)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:util/store/object-id
                #:oid))
(in-package :screenshotbot/login/test-saml)


(util/fiveam:def-suite)

(defvar *xml*
  "<md:EntityDescriptor xmlns='urn:oasis:names:tc:SAML:2.0:metadata' xmlns:md='urn:oasis:names:tc:SAML:2.0:metadata' xmlns:saml='urn:oasis:names:tc:SAML:2.0:assertion' xmlns:ds='http://www.w3.org/2000/09/xmldsig#' entityID='http://localhost:8080/realms/master'><md:IDPSSODescriptor WantAuthnRequestsSigned='true' protocolSupportEnumeration='urn:oasis:names:tc:SAML:2.0:protocol'><md:KeyDescriptor use='signing'><ds:KeyInfo><ds:KeyName>KGh_gBNYNUgpON0_vz_AjVwJhAcRGZSSwWELSD6Al-Q</ds:KeyName><ds:X509Data><ds:X509Certificate>MIICmzCCAYMCBgGeV3VXnzANBgkqhkiG9w0BAQsFADARMQ8wDQYDVQQDDAZtYXN0ZXIwHhcNMjYwNTI0MDA0NzQ4WhcNMzYwNTI0MDA0OTI4WjARMQ8wDQYDVQQDDAZtYXN0ZXIwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC1BzweaB4nAZ903zJXA0EMTi/vhq0LUwHM+8hiJXSsZ0IzKCCkoJymS0txSirRekiItw31/YV9kZhWY0OsQM9YmrCoq4HeOgA/B4Xy60zTzalEIlleRIYfQPdU3X7795LY2Ajgxu80AwAj62yP9VpDwaAaVYD9LECwNGL9Z6WGoSrI1NSHzFbaVAKMezrnHEhiJ5MZjjNCIaMU/KOlkUCEBofO7jMsaiER4IkSJhEH8UfQueHr3Vneu8O7n1yfg0CyNLZHF4NnDioPpIFt+O1yvCtEVuDqVoGkqn0JwajPlhOwX6Nrfe0u/MrSEiiwmUXY5droAve2CZW7T3GV1lzLAgMBAAEwDQYJKoZIhvcNAQELBQADggEBACeVAiUCvWzblxFCXgR6Lt/MAIXiQad5O4hzvwfq1d/QmZcd1f63jNbMEY+LKQhdd8A+zszKRsckJ6HcuK17EhEgFA2QM8aBrMohs13LDNoV7KhbsVQRy2wVwcEsWtoolf78eGEN7ydHpvNkBS3X+MUm34miEoUetMg4cTi8fQ0OixjZy/4O/01qa2j3Mebbrip8hk38CA4ltjvsoZzw54Y162WUqy3/jeLbhcRTO8GmcDkJlVpqSu/IEaC5T0jvw5v/5ZMlEFpJtIn0AYbyW0Ck47FiKU6FukkXrVlbfvgVLroa2IDwytXkBhPMer7LM+g2CoZc1tUx38JTqQ9nkSY=</ds:X509Certificate></ds:X509Data></ds:KeyInfo></md:KeyDescriptor><md:ArtifactResolutionService Binding='urn:oasis:names:tc:SAML:2.0:bindings:SOAP' Location='http://localhost:8080/realms/master/protocol/saml/resolve' index='0'></md:ArtifactResolutionService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Artifact' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:SOAP' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:NameIDFormat>urn:oasis:names:tc:SAML:2.0:nameid-format:persistent</md:NameIDFormat><md:NameIDFormat>urn:oasis:names:tc:SAML:2.0:nameid-format:transient</md:NameIDFormat><md:NameIDFormat>urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified</md:NameIDFormat><md:NameIDFormat>urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress</md:NameIDFormat><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:SOAP' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Artifact' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService></md:IDPSSODescriptor></md:EntityDescriptor>")

(def-fixture state ()
  (with-test-store ()
    (with-installation ()
     (let ((company (make-instance 'company)))
       (&body)))))

(test simple-parsing
  (with-fixture state ()
   (is
    (equal
     "http://localhost:8080/realms/master/protocol/saml"
     (parse-xml *xml*)))))

(test creating-settings-builder
  (with-fixture state ()
   (let ((self (make-instance 'saml-auth-provider)))
     (finishes
       (create-settings-builder-for-xml company *xml*))
     (finishes
       (authn-request-get-encoded-authn-request
        (make-authn-request (create-settings-builder-for-xml company *xml*)))))))


(test ensures-creates-new-saml-user
  (with-fixture state ()
    (is (not (null
              (find-user-for-sso company "foo@gmail.com"))))))

(test make-cert-pair
  (with-fixture state ()
    (assert-that
     (make-cert-pair)
     (has-typep 'string))
    (assert-that
     (find-or-create-saml-cert company)
     (is-not-null))
    (is (eql
         (find-or-create-saml-cert company)
         (find-or-create-saml-cert company)))))

(test default-saml-provider
  (with-fixture state ()
    (is (eql nil (default-saml-auth-provider)))
    (let ((one (make-instance 'saml-auth-provider))
         (two (make-instance 'saml-auth-provider
                             :defaultp t))
         (three (make-instance 'saml-auth-provider
                               :defaultp nil))
         (four (make-instance 'saml-auth-provider
                              :defaultp nil)))
      (is (eql two (default-saml-auth-provider))))))

(test dont-allow-session-replays
  (with-fixture state ()
    (with-fake-request ()
     (auth:with-sessions ()
       (let* ((relay-state (make-relay-state
                            (make-instance 'saml-auth-provider
                                           :metadata-xml *xml*)))
              (id (relay-state-id relay-state)))
         (is
          (eql relay-state
               (relay-state-for-id id)))
         (signals relay-state-expired
           (relay-state-for-id id)))))))

(test relay-state-for-id-handles-nil
  (with-fixture state ()
    (is
     (eql nil
          (relay-state-for-id nil)))))

(test clear-relay-state-happy-path
  (with-fixture state ()
    (with-fake-request ()
     (auth:with-sessions ()
       (let* ((relay-state (make-relay-state
                            (make-instance 'saml-auth-provider
                                           :metadata-xml *xml*)))
              (id (relay-state-id relay-state)))
         (clear-relay-states))))))

(defun make-fake-saml-response (issuer)
  (base64:string-to-base64-string
   (format nil "<samlp:Response xmlns:samlp='urn:oasis:names:tc:SAML:2.0:protocol' xmlns:saml='urn:oasis:names:tc:SAML:2.0:assertion' ID='_resp1' Version='2.0' IssueInstant='2026-10-14T00:00:00Z'><saml:Issuer>~a</saml:Issuer><samlp:Status><samlp:StatusCode Value='urn:oasis:names:tc:SAML:2.0:status:Success'/></samlp:Status></samlp:Response>"
           issuer)))

(test find-saml-auth-provider-for-response
  (with-fixture state ()
    (let ((other (make-instance 'saml-auth-provider
                                :company company
                                :metadata-xml (str:replace-all
                                               "http://localhost:8080/realms/master'"
                                               "http://example.com/other'"
                                               *xml*)))
          (provider (make-instance 'saml-auth-provider
                                   :company company
                                   :metadata-xml *xml*)))
      (declare (ignore other))
      (is (eql provider
               (find-saml-auth-provider-for-response
                (make-fake-saml-response "http://localhost:8080/realms/master")
                "https://example.com/sso/saml/callback")))
      (is (eql nil
               (find-saml-auth-provider-for-response
                (make-fake-saml-response "http://example.com/unknown")
                "https://example.com/sso/saml/callback"))))))

(test valid-relay-state-p
  (is-true (valid-relay-state-p "234343_34334"))
  ;; One of our customers configured it this way:
  (is-false (valid-relay-state-p "https://foo.screenshotbot.io"))
  (is-false (valid-relay-state-p "sdfds234342_34234234sdfsfs")))

(defun cert-body (pem)
  "The base64 body of a PEM certificate, as it appears in the metadata XML"
  (str:join ""
            (remove-if (lambda (line)
                         (or (str:emptyp line)
                             (str:starts-with-p "-----" line)))
                       (str:lines pem))))

(test sp-metadata-uses-company-from-oid
  (with-fixture state ()
    (let ((other-company (make-instance 'company)))
      (find-or-create-saml-cert other-company)
      (with-fake-request ()
        ;; No logged in user: this is how the IdP fetches it
        (let ((xml (sp-metadata :oid (oid company))))
          (is (str:containsp
               (cert-body (saml-public-cert (find-or-create-saml-cert company)))
               xml))
          (is (not (str:containsp
                    (cert-body (saml-public-cert (find-or-create-saml-cert other-company)))
                    xml))))))))

(test singleton-sp-metadata
  (with-fixture state ()
    (with-fake-request ()
      (let ((xml (singleton-sp-metadata)))
        (is (str:containsp
             (cert-body (saml-public-cert
                         (find-or-create-saml-cert
                          (get-singleton-company *installation*))))
             xml))
        (is (str:containsp
             (format nil "entityID=\"~a\"" (entity-id))
             xml))))))

(def-fixture test-login ()
  (with-fake-request ()
    (auth:with-sessions ()
      (let* ((admin (make-user :email "admin@example.com"))
             (provider (make-instance 'saml-auth-provider
                                      :company company
                                      :metadata-xml *xml*))
             (relay-state (make-relay-state provider
                                            :tested-by admin
                                            :redirect "/sso")))
        (&body)))))

(test failed-test-login-is-recorded
  (with-fixture state ()
    (with-fixture test-login ()
      (signals hex:redirected
        (handle-saml-response
         (make-saml-response (relay-state-settings relay-state)
                             "https://example.com/sso/saml/callback"
                             (make-fake-saml-response "http://localhost:8080/realms/master"))
         relay-state))
      (let ((result (last-saml-test-result provider)))
        (is (eql admin (saml-test-result-tested-by result)))
        (is (eql nil (saml-test-result-email result)))
        (is (str:non-empty-string-p (saml-test-result-error result)))))))

(test invalid-response-without-test-still-errors
  (with-fixture state ()
    (with-fixture test-login ()
      (let ((relay-state (make-relay-state provider)))
        (signals simple-error
          (handle-saml-response
           (make-saml-response (relay-state-settings relay-state)
                               "https://example.com/sso/saml/callback"
                               (make-fake-saml-response "http://localhost:8080/realms/master"))
           relay-state)))
      (is (eql nil (last-saml-test-result provider))))))

(test successful-test-login-does-not-log-in
  (with-fixture state ()
    (with-fixture test-login ()
      (let ((session (auth:current-session)))
        (signals hex:redirected
          (finish-test-login relay-state :email "foo@example.com"))
        (is (eql session (auth:current-session)))
        (is (eql nil (auth:current-user))))
      (is (eql nil (auth:find-user *installation* :email "foo@example.com")))
      (is (equal "foo@example.com"
                 (saml-test-result-email (last-saml-test-result provider))))
      (is (eql nil (saml-test-result-error (last-saml-test-result provider)))))))

(test last-saml-test-result-is-the-latest
  (with-fixture state ()
    (with-fixture test-login ()
      (signals hex:redirected
        (finish-test-login relay-state :error-message "bad"))
      ;; Same second as the previous result
      (signals hex:redirected
        (finish-test-login relay-state :email "foo@example.com"))
      (is (equal "foo@example.com"
                 (saml-test-result-email (last-saml-test-result provider)))))))

(defclass multi-org-installation (multi-org-feature
                                  installation)
  ())

(def-fixture activation ()
  (with-fake-request ()
    (auth:with-sessions ()
      (let* ((company (get-singleton-company *installation*))
             (admin (make-user :email "admin@example.com"))
             (provider (make-instance 'saml-auth-provider
                                      :company company
                                      :metadata-xml *xml*)))
        (setf (roles:user-role company admin) 'roles:admin)
        (flet ((record-test (&key (email "admin@example.com")
                               error-message
                               (tested-by admin)
                               (ts (get-universal-time)))
                 (make-instance 'saml-test-result
                                :provider provider
                                :tested-by tested-by
                                :email (unless error-message email)
                                :error-message error-message
                                :ts ts)))
          (declare (ignorable #'record-test))
          (&body))))))

(test activation-requires-a-test
  (with-fixture state ()
    (with-fixture activation ()
      (is (equal "Test this provider before activating it"
                 (activation-blocker provider admin)))
      (record-test :error-message "bad audience")
      (is (equal "The last test failed"
                 (activation-blocker provider admin))))))

(test activation-requires-own-recent-test
  (with-fixture state ()
    (with-fixture activation ()
      (let ((other-admin (make-user :email "other@example.com")))
        (setf (roles:user-role company other-admin) 'roles:admin)
        (record-test :tested-by other-admin)
        (is (equal "Test this provider yourself before activating it"
                   (activation-blocker provider admin))))
      (record-test :ts (- (get-universal-time) (* 2 3600)))
      (is (str:containsp "more than an hour old"
                         (activation-blocker provider admin)))
      ;; It doesn't matter who the IdP signed in as: a non-admin, or
      ;; someone who doesn't exist yet
      (make-user :email "member@example.com")
      (record-test :email "member@example.com")
      (is (eql nil (activation-blocker provider admin)))
      (record-test :email "new@example.com")
      (is (eql nil (activation-blocker provider admin)))
      (record-test)
      (is (eql nil (activation-blocker provider admin))))))

(test activate-replaces-the-default
  (with-fixture state ()
    (with-fixture activation ()
      (let ((old (make-instance 'saml-auth-provider
                                :company company
                                :defaultp t)))
        (record-test)
        (activate-saml-provider provider admin)
        (is (eql provider (default-saml-auth-provider)))
        (is-true (defaultp provider))
        (is-false (defaultp old))
        (is (equal "This provider is already active"
                   (activation-blocker provider admin)))
        (deactivate-saml-provider provider)
        (is (eql nil (default-saml-auth-provider)))))))

(test activate-refuses-when-blocked
  (with-fixture state ()
    (with-fixture activation ()
      (let ((old (make-instance 'saml-auth-provider
                                :company company
                                :defaultp t)))
        (signals simple-error
          (activate-saml-provider provider admin))
        (is (eql old (default-saml-auth-provider)))))))

(test activation-is-only-for-single-company-installs
  (with-test-store ()
    (let ((*installation* (make-instance 'multi-org-installation)))
      (let* ((company (make-instance 'company))
             (admin (make-user :email "admin@example.com"))
             (provider (make-instance 'saml-auth-provider
                                      :company company)))
        (setf (roles:user-role company admin) 'roles:admin)
        (is (str:containsp "single-company"
                           (activation-blocker provider admin)))))))

(test edit-changes-the-provider
  (with-fixture state ()
    (let ((provider (make-instance 'saml-auth-provider
                                   :company company
                                   :name "Old"
                                   :idp-metadata-url "https://old.example.com/metadata"
                                   :metadata-xml *xml*)))
      (edit-saml-provider provider :name "New"
                                   :idp-metadata-url "https://old.example.com/metadata")
      (is (equal "New" (saml-name provider)))
      ;; Only the name changed, so tests still count
      (is (eql nil (saml-edited-at provider)))
      (is (equal *xml* (screenshotbot/login/saml::%metadata-xml provider)))
      (edit-saml-provider provider :name "New"
                                   :idp-metadata-url "https://new.example.com/metadata")
      (is (equal "https://new.example.com/metadata" (saml-idp-metadata-url provider)))
      ;; We must not keep using the old IdP's metadata
      (is (eql nil (screenshotbot/login/saml::%metadata-xml provider)))
      (is (integerp (saml-edited-at provider))))))

(test cant-change-the-idp-of-the-active-provider
  (with-fixture state ()
    (let ((provider (make-instance 'saml-auth-provider
                                   :company company
                                   :name "Old"
                                   :idp-metadata-url "https://old.example.com/metadata"
                                   :defaultp t)))
      (signals simple-error
        (edit-saml-provider provider :name "Old"
                                     :idp-metadata-url "https://new.example.com/metadata"))
      (is (equal "https://old.example.com/metadata" (saml-idp-metadata-url provider)))
      ;; Renaming is fine
      (edit-saml-provider provider :name "New"
                                   :idp-metadata-url "https://old.example.com/metadata")
      (is (equal "New" (saml-name provider))))))

(test editing-the-idp-invalidates-tests
  (with-fixture state ()
    (with-fixture activation ()
      (record-test :ts (- (get-universal-time) 10))
      (is (eql nil (activation-blocker provider admin)))
      (edit-saml-provider provider :name "x" :idp-metadata-url "https://new.example.com/metadata")
      (is (equal "The provider changed since the last test, test it again"
                 (activation-blocker provider admin)))
      ;; A test in the same second as the edit doesn't count either
      (record-test)
      (is (equal "The provider changed since the last test, test it again"
                 (activation-blocker provider admin))))))

(test edited-at-on-older-providers
  (with-fixture state ()
    (let ((provider (make-instance 'saml-auth-provider :company company)))
      (slot-makunbound provider 'screenshotbot/login/saml::%edited-at)
      (is (eql nil (saml-edited-at provider))))))

(test editing-a-provider-without-a-url
  (with-fixture state ()
    ;; Created at the REPL, with only the XML
    (let ((provider (make-instance 'saml-auth-provider
                                   :company company
                                   :metadata-xml *xml*)))
      (is (eql nil (saml-idp-metadata-url provider)))
      (edit-saml-provider provider :name "x" :idp-metadata-url "https://new.example.com/metadata")
      (is (equal "https://new.example.com/metadata" (saml-idp-metadata-url provider))))))

(test delete-removes-the-provider-and-its-test-results
  (with-fixture state ()
    (with-fixture activation ()
      (record-test)
      (record-test :error-message "bad")
      (delete-saml-provider provider)
      (is-true (bknr.indices:object-destroyed-p provider))
      (is (fset:empty? (saml-auth-providers-for-company company)))
      (is (fset:empty? (saml-test-results-for-provider provider)))
      (is (eql nil (bknr.datastore:store-objects-with-class
                    'screenshotbot/login/saml::saml-test-result))))))

(test cant-delete-the-active-provider
  (with-fixture state ()
    (let ((provider (make-instance 'saml-auth-provider
                                   :company company
                                   :defaultp t)))
      (signals simple-error
        (delete-saml-provider provider))
      (is (eql provider (default-saml-auth-provider))))))

(test login-for-a-deleted-provider-fails
  (with-fixture state ()
    (with-fixture test-login ()
      (let ((relay-state (make-relay-state provider)))
        (delete-saml-provider provider)
        (is (str:containsp
             "was deleted"
             (handler-case
                 (progn
                   (handle-saml-response
                    (make-saml-response (relay-state-settings relay-state)
                                        "https://example.com/sso/saml/callback"
                                        (make-fake-saml-response "http://localhost:8080/realms/master"))
                    relay-state)
                   "no error")
               (simple-error (e)
                 (princ-to-string e)))))))))

(test entity-id-does-not-change
  ;; Customers have this as their IdP's Audience URI. If this test
  ;; fails, you're about to break their logins. See ENTITY-ID.
  (with-fixture state ()
    (is (equal "/saml/metadata"
               (quri:uri-path (quri:uri (entity-id)))))
    (is (equal (quri:uri-host (quri:uri (installation-domain *installation*)))
               (quri:uri-host (quri:uri (entity-id)))))))

(def-fixture site-admin-login (&key (site-admins-can-use-sso-p t)
                                    (email "staff@example.com"))
  (with-fake-request ()
    (auth:with-sessions ()
      (cl-mock:with-mocks ()
        (cl-mock:if-called 'screenshotbot/installation:site-admins-can-use-sso-p
                           (lambda (installation)
                             (declare (ignore installation))
                             site-admins-can-use-sso-p))
        (cl-mock:if-called 'screenshotbot/login/saml::saml-response-get-attributes
                           (lambda (resp) (declare (ignore resp)) nil))
        (cl-mock:if-called 'screenshotbot/login/saml::saml-response-get-name-id
                           (lambda (resp) (declare (ignore resp)) email))
        (cl-mock:if-called 'screenshotbot/login/saml::get-idp-entity-id
                           (lambda (settings) (declare (ignore settings)) "entity-id"))
        (let* ((staff (make-user :email "staff@example.com"))
               (provider (make-instance 'saml-auth-provider
                                        :company company
                                        :metadata-xml *xml*))
               (relay-state (make-relay-state provider)))
          (setf (screenshotbot/model/user:adminp staff) t)
          (&body))))))

(defun saml-login (relay-state)
  "Log in with a (mocked) validated SAML response. Returns the response
HTML, or NIL if we redirected."
  (handler-case
      (markup:write-html
       (screenshotbot/login/saml::process-validated-callback :resp relay-state))
    (hex:redirected ()
      nil)))

(test site-admin-cant-sign-in-with-saml-if-not-allowed
  (with-fixture state ()
    (with-fixture site-admin-login (:site-admins-can-use-sso-p nil)
      (is (str:containsp "sign in with SAML on this installation"
                         (saml-login relay-state)))
      (is (eql nil (auth:current-user)))
      ;; We didn't add the site admin to the company
      (is (eql nil (roles:user-role company staff))))))

(test site-admin-can-sign-in-with-saml-if-allowed
  (with-fixture state ()
    (with-fixture site-admin-login (:site-admins-can-use-sso-p t)
      (is (eql nil (saml-login relay-state)))
      (is (eql staff (auth:current-user))))))

(test other-users-can-sign-in-with-saml-when-site-admins-cant
  (with-fixture state ()
    (with-fixture site-admin-login (:site-admins-can-use-sso-p nil
                                    :email "member@example.com")
      (is (eql nil (saml-login relay-state)))
      (is (equal "member@example.com"
                 (auth:user-email (auth:current-user)))))))

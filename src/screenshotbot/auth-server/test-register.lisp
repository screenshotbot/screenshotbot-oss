;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-register
  (:use #:cl
        #:fiveam)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:screenshotbot/auth-server/errors
                #:with-oauth-json-errors)
  (:import-from #:screenshotbot/auth-server/model
                #:find-oauth-client
                #:oauth-client-redirect-uris
                #:oauth-client-scopes
                #:oauth-client-self-registered-p
                #:public-client-p
                #:register-oauth-client)
  (:import-from #:screenshotbot/auth-server/register
                #:%register
                #:validate-redirect-uris!)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:documentation "Tests for RFC 7591 dynamic client registration."))
(in-package :screenshotbot/auth-server/test-register)

(util/fiveam:def-suite)

(defun decode-json (string)
  (let ((json:*json-identifier-name-to-lisp* #'identity)
        (json:*identifier-name-to-key* #'identity))
    (json:decode-json-from-string string)))

(defun field (response name)
  (assoc-value response name :test #'equal))

(defparameter +minimal+
  "{\"redirect_uris\": [\"https://claude.ai/api/mcp/auth_callback\"]}")

(defun post-registration (json)
  "Drive the endpoint with a raw JSON body, returning the decoded reply
and the status."
  (with-fake-request (:script-name "/oauth/register")
    (cl-mock:with-mocks ()
      (cl-mock:if-called 'hunchentoot:raw-post-data
                         (lambda (&rest args) (declare (ignore args)) json))
      (let ((body (with-oauth-json-errors ()
                    (%register))))
        (values (decode-json body) (hunchentoot:return-code*))))))

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:company company :user user)
      (&body))))

;; ----------------------------------------------------------------------
;; The happy path
;; ----------------------------------------------------------------------

(test a-client-can-register-itself-and-gets-a-usable-client-id
  (with-fixture state ()
    (multiple-value-bind (response status) (post-registration +minimal+)
      (is (equal 201 status))
      (let ((client-id (field response "client_id")))
        (is-true client-id)
        ;; And it can be found again, which is the only thing the client
        ;; will do with it.
        (let ((client (find-oauth-client client-id)))
          (is-true client)
          (assert-that (oauth-client-redirect-uris client)
                       (has-item "https://claude.ai/api/mcp/auth_callback")))))))

(test a-registered-client-is-marked-self-registered
  "This is what makes the consent screen render it differently. If it were
ever to default the other way, open registration would silently become a
phishing tool."
  (with-fixture state ()
    (let ((client (find-oauth-client
                   (field (post-registration +minimal+) "client_id"))))
      (is-true (oauth-client-self-registered-p client)))))

(test a-client-created-from-lisp-is-not-marked-self-registered
  (with-fixture state ()
    (is-false (oauth-client-self-registered-p
               (register-oauth-client :client-id "vetted")))))

(test asking-for-no-auth-method-yields-a-public-client
  "MCP clients send token_endpoint_auth_method=none and rely on PKCE."
  (with-fixture state ()
    (let* ((response (post-registration
                      "{\"redirect_uris\": [\"https://claude.ai/cb\"],
                        \"token_endpoint_auth_method\": \"none\"}"))
           (client (find-oauth-client (field response "client_id"))))
      (is-true (public-client-p client))
      (is (equal "none" (field response "token_endpoint_auth_method")))
      ;; A public client has no secret to hand back.
      (is-false (field response "client_secret")))))

(test the-default-auth-method-produces-a-confidential-client
  "RFC 7591 §2 defaults to client_secret_basic when the field is absent."
  (with-fixture state ()
    (let* ((response (post-registration +minimal+))
           (client (find-oauth-client (field response "client_id"))))
      (is-false (public-client-p client))
      (is-true (field response "client_secret"))
      ;; 0 means never; we have no rotation story and should not imply one.
      (is (equal 0 (field response "client_secret_expires_at"))))))

(test the-client-name-is-recorded-for-the-consent-screen
  (with-fixture state ()
    (let ((response (post-registration
                     "{\"redirect_uris\": [\"https://claude.ai/cb\"],
                       \"client_name\": \"Claude\"}")))
      (is (equal "Claude" (field response "client_name"))))))

(test a-lone-redirect-uri-string-is-accepted
  "Some clients send a bare string instead of an array. Refusing would be
defensible, but the failure lands on a user connecting an app, a long way
from anyone who can read the spec."
  (with-fixture state ()
    (multiple-value-bind (response status)
        (post-registration "{\"redirect_uris\": \"https://claude.ai/cb\"}")
      (is (equal 201 status))
      (assert-that (oauth-client-redirect-uris
                    (find-oauth-client (field response "client_id")))
                   (has-item "https://claude.ai/cb")))))

;; ----------------------------------------------------------------------
;; Redirect URI rules
;; ----------------------------------------------------------------------

(test a-registration-without-a-redirect-uri-is-refused
  (with-fixture state ()
    (is (equal "invalid_redirect_uri"
               (field (post-registration "{}") "error")))
    (is (equal "invalid_redirect_uri"
               (field (post-registration "{\"redirect_uris\": []}") "error")))))

(test plain-http-to-a-real-host-is-refused
  "The authorization code would cross the network in the clear."
  (with-fixture state ()
    (is (equal "invalid_redirect_uri"
               (field (post-registration
                       "{\"redirect_uris\": [\"http://claude.ai/cb\"]}")
                      "error")))))

(test loopback-http-is-allowed
  "RFC 8252 §7.3 carves this out because it never leaves the machine."
  (with-fixture state ()
    (dolist (uri '("http://127.0.0.1:1234/callback" "http://localhost/cb"))
      (is (equal (list uri) (validate-redirect-uris! (list uri)))))))

(test relative-and-fragment-bearing-redirect-uris-are-refused
  (with-fixture state ()
    (dolist (bad '(("/cb") ("cb") ("https://claude.ai/cb#frag")))
      (is (equal "invalid_redirect_uri"
                 (field (post-registration
                         (format nil "{\"redirect_uris\": [\"~a\"]}" (first bad)))
                        "error"))
          "expected ~s to be refused" (first bad)))))

;; ----------------------------------------------------------------------
;; Metadata rules
;; ----------------------------------------------------------------------

(test an-unsupported-grant-type-is-refused-by-name
  (with-fixture state ()
    (let ((response (post-registration
                     "{\"redirect_uris\": [\"https://claude.ai/cb\"],
                       \"grant_types\": [\"password\"]}")))
      (is (equal "invalid_client_metadata" (field response "error")))
      (is-true (str:containsp "password" (field response "error_description"))))))

(test only-the-code-response-type-is-accepted
  (with-fixture state ()
    (is (equal "invalid_client_metadata"
               (field (post-registration
                       "{\"redirect_uris\": [\"https://claude.ai/cb\"],
                         \"response_types\": [\"token\"]}")
                      "error")))))

(test an-unsupported-auth-method-is-refused
  (with-fixture state ()
    (is (equal "invalid_client_metadata"
               (field (post-registration
                       "{\"redirect_uris\": [\"https://claude.ai/cb\"],
                         \"token_endpoint_auth_method\": \"private_key_jwt\"}")
                      "error")))))

(test a-client-cannot-register-for-a-scope-we-do-not-issue
  (with-fixture state ()
    (let ((response (post-registration
                     "{\"redirect_uris\": [\"https://claude.ai/cb\"],
                       \"scope\": \"api:read admin:everything\"}")))
      (is (equal "invalid_client_metadata" (field response "error")))
      (is-true (str:containsp "admin:everything"
                              (field response "error_description"))))))

(test a-requested-scope-is-what-the-client-is-allowed
  (with-fixture state ()
    (let ((client (find-oauth-client
                   (field (post-registration
                           "{\"redirect_uris\": [\"https://claude.ai/cb\"],
                             \"scope\": \"api:read\"}")
                          "client_id"))))
      (is (equal '("api:read") (oauth-client-scopes client))))))

(test registering-with-no-scope-gets-the-least-privileged-set
  (with-fixture state ()
    (is (equal '("profile")
               (oauth-client-scopes
                (find-oauth-client
                 (field (post-registration +minimal+) "client_id")))))))

;; ----------------------------------------------------------------------
;; Malformed input
;; ----------------------------------------------------------------------

(test a-body-that-is-not-json-is-refused
  (with-fixture state ()
    (is (equal "invalid_client_metadata"
               (field (post-registration "not json at all") "error")))
    (is (equal "invalid_client_metadata"
               (field (post-registration "") "error")))))

(test a-redirect-uris-value-of-the-wrong-shape-is-refused
  (with-fixture state ()
    (is (equal "invalid_client_metadata"
               (field (post-registration "{\"redirect_uris\": [1, 2]}") "error")))))

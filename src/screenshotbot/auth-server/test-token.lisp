;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-token
  (:use #:cl
        #:fiveam)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:core/api/model/api-key
                #:%find-api-key
                #:decode-api-token)
  (:import-from #:screenshotbot/auth-server/errors
                #:with-oauth-json-errors)
  (:import-from #:screenshotbot/auth-server/model
                #:oauth-client-secret
                #:*authorization-code-ttl*
                #:*device-code-ttl*
                #:*refresh-token-ttl*
                #:approve-device-request
                #:code-string
                #:deny-device-request
                #:device-code-string
                #:device-status
                #:grant-revoked-p
                #:make-device-request
                #:make-oauth-code
                #:make-refresh-token
                #:oauth-grant
                #:refresh-token-revoked-p
                #:refresh-token-string
                #:register-oauth-client
                #:revoke-grant)
  (:import-from #:screenshotbot/auth-server/pkce
                #:random-token
                #:s256-challenge)
  (:import-from #:screenshotbot/auth-server/token
                #:+device-code-grant-type+
                #:%token)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/auth-server/test-token)

(util/fiveam:def-suite)

(defparameter +redirect-uri+ "http://127.0.0.1:43219/callback")

(defun decode-json (string)
  "Decode with the JSON member names left exactly as they came off the wire."
  (let ((json:*json-identifier-name-to-lisp* #'identity)
        (json:*identifier-name-to-key* #'identity))
    (json:decode-json-from-string string)))

(defun field (response name)
  (assoc-value response name :test #'equal))

(defun post-token (&rest params)
  "Drive the token endpoint the way a real POST would, and decode the reply."
  (with-fake-request (:script-name "/oauth/token")
    (loop for (key value) on params by #'cddr
          if value
            do (setf (hunchentoot:post-parameter hunchentoot:*request* key)
                     value))
    (decode-json
     (with-oauth-json-errors ()
       (%token)))))

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:company company :user user)
      (let* ((verifier (random-token 32))
             (client (register-oauth-client
                      :client-id "test-client"
                      :name "Test Client"
                      :redirect-uris (list "http://127.0.0.1/callback")
                      :scopes (list "profile" "api:read" "api:write"))))
        (flet ((make-grant (&key (scopes '("api:read")))
                 (make-instance 'oauth-grant
                                :client client
                                :user user
                                :company company
                                :scopes scopes))
               (make-code (grant &key (redirect-uri +redirect-uri+)
                                   (challenge-verifier verifier))
                 (make-oauth-code
                  :grant grant
                  :redirect-uri redirect-uri
                  :challenge (when challenge-verifier
                               (s256-challenge challenge-verifier))
                  :challenge-method (when challenge-verifier "S256"))))
          (&body))))))

;; ----------------------------------------------------------------------
;; grant_type=authorization_code
;; ----------------------------------------------------------------------

(test authorization-code-exchange-returns-a-usable-bearer-token
  (with-fixture state ()
    (let* ((grant (make-grant :scopes '("api:read")))
           (response (post-token "grant_type" "authorization_code"
                                 "client_id" "test-client"
                                 "code" (code-string (make-code grant))
                                 "redirect_uri" +redirect-uri+
                                 "code_verifier" verifier)))
      (is (equal "Bearer" (field response "token_type")))
      (is (equal "api:read" (field response "scope")))
      (is (< 0 (field response "expires_in")))
      (is-true (field response "refresh_token"))
      ;; And the token really does authenticate.
      (multiple-value-bind (key)
          (decode-api-token (field response "access_token"))
        (is (eq grant
                (screenshotbot/auth-server/model:access-token-grant
                 (%find-api-key key))))))))

(test authorization-code-requires-the-matching-pkce-verifier
  (with-fixture state ()
    (let ((code (code-string (make-code (make-grant)))))
      (let ((response (post-token "grant_type" "authorization_code"
                                  "client_id" "test-client"
                                  "code" code
                                  "redirect_uri" +redirect-uri+
                                  "code_verifier" (random-token 32))))
        (is (equal "invalid_grant" (field response "error")))
        (is-false (field response "access_token"))))))

(test authorization-code-rejects-a-missing-verifier
  (with-fixture state ()
    (let ((response (post-token "grant_type" "authorization_code"
                                "client_id" "test-client"
                                "code" (code-string (make-code (make-grant)))
                                "redirect_uri" +redirect-uri+)))
      (is (equal "invalid_grant" (field response "error"))))))

(test replaying-an-authorization-code-revokes-the-whole-grant
  "RFC 6819 §5.2.1.1: a code used twice has leaked, and we can't tell
which caller was the attacker, so neither of them keeps access."
  (with-fixture state ()
    (let* ((grant (make-grant))
           (code (code-string (make-code grant))))
      (flet ((exchange ()
               (post-token "grant_type" "authorization_code"
                           "client_id" "test-client"
                           "code" code
                           "redirect_uri" +redirect-uri+
                           "code_verifier" verifier)))
        (is-true (field (exchange) "access_token"))
        (is-false (grant-revoked-p grant))
        (let ((second (exchange)))
          (is (equal "invalid_grant" (field second "error")))
          (is-false (field second "access_token")))
        (is-true (grant-revoked-p grant))))))

(test a-failed-pkce-check-still-burns-the-code
  "Otherwise an attacker holding a stolen code could brute force the verifier."
  (with-fixture state ()
    (let ((code (code-string (make-code (make-grant)))))
      (is (equal "invalid_grant"
                 (field (post-token "grant_type" "authorization_code"
                                    "client_id" "test-client"
                                    "code" code
                                    "redirect_uri" +redirect-uri+
                                    "code_verifier" (random-token 32))
                        "error")))
      ;; The correct verifier no longer helps.
      (is (equal "invalid_grant"
                 (field (post-token "grant_type" "authorization_code"
                                    "client_id" "test-client"
                                    "code" code
                                    "redirect_uri" +redirect-uri+
                                    "code_verifier" verifier)
                        "error"))))))

(test authorization-code-rejects-a-different-redirect-uri
  (with-fixture state ()
    (is (equal "invalid_grant"
               (field (post-token "grant_type" "authorization_code"
                                  "client_id" "test-client"
                                  "code" (code-string (make-code (make-grant)))
                                  "redirect_uri" "http://127.0.0.1:99/callback"
                                  "code_verifier" verifier)
                      "error")))))

(test authorization-code-cannot-be-redeemed-by-another-client
  (with-fixture state ()
    (register-oauth-client :client-id "other-client"
                           :redirect-uris (list "http://127.0.0.1/callback"))
    (is (equal "invalid_grant"
               (field (post-token "grant_type" "authorization_code"
                                  "client_id" "other-client"
                                  "code" (code-string (make-code (make-grant)))
                                  "redirect_uri" +redirect-uri+
                                  "code_verifier" verifier)
                      "error")))))

(test expired-authorization-codes-are-rejected
  (with-fixture state ()
    (let ((code (let ((*authorization-code-ttl* -1))
                  (code-string (make-code (make-grant))))))
      (is (equal "invalid_grant"
                 (field (post-token "grant_type" "authorization_code"
                                    "client_id" "test-client"
                                    "code" code
                                    "redirect_uri" +redirect-uri+
                                    "code_verifier" verifier)
                        "error"))))))

(test an-unknown-authorization-code-is-rejected
  (with-fixture state ()
    (is (equal "invalid_grant"
               (field (post-token "grant_type" "authorization_code"
                                  "client_id" "test-client"
                                  "code" "not-a-real-code"
                                  "redirect_uri" +redirect-uri+
                                  "code_verifier" verifier)
                      "error")))))

(test a-revoked-grant-cannot-produce-tokens
  (with-fixture state ()
    (let* ((grant (make-grant))
           (code (code-string (make-code grant))))
      (revoke-grant grant)
      (is (equal "invalid_grant"
                 (field (post-token "grant_type" "authorization_code"
                                    "client_id" "test-client"
                                    "code" code
                                    "redirect_uri" +redirect-uri+
                                    "code_verifier" verifier)
                        "error"))))))

;; ----------------------------------------------------------------------
;; Client authentication
;; ----------------------------------------------------------------------

(test an-unknown-client-is-rejected
  (with-fixture state ()
    (is (equal "invalid_client"
               (field (post-token "grant_type" "authorization_code"
                                  "client_id" "no-such-client")
                      "error")))))

(test a-missing-client-id-is-rejected
  (with-fixture state ()
    (is (equal "invalid_client"
               (field (post-token "grant_type" "authorization_code")
                      "error")))))

(test a-confidential-client-must-present-its-secret
  (with-fixture state ()
    (register-oauth-client :client-id "conf-client" :public nil
                           :redirect-uris (list "http://127.0.0.1/callback"))
    ;; No Basic auth header, so no secret.
    (is (equal "invalid_client"
               (field (post-token "grant_type" "authorization_code"
                                  "client_id" "conf-client")
                      "error")))))

(test a-confidential-client-with-the-right-secret-is-accepted
  "The negative test above passes even if the comparison is broken, so the
success path needs its own."
  (with-fixture state ()
    (let* ((confidential (register-oauth-client
                          :client-id "conf-client" :public nil
                          :redirect-uris (list "http://127.0.0.1/callback")))
           (grant (make-instance 'oauth-grant
                                 :client confidential
                                 :user user
                                 :company company
                                 :scopes '("api:read")))
           (code (code-string (make-code grant :challenge-verifier nil))))
      (cl-mock:with-mocks ()
        ;; HTTP Basic is how RFC 6749 §2.3.1 says a confidential client
        ;; authenticates; WITH-FAKE-REQUEST has no way to set the header.
        (cl-mock:if-called 'hunchentoot:authorization
                           (lambda ()
                             (values "conf-client"
                                     (oauth-client-secret confidential))))
        (let ((response (post-token "grant_type" "authorization_code"
                                    "code" code
                                    "redirect_uri" +redirect-uri+)))
          (is (equal "Bearer" (field response "token_type")))
          (is-true (field response "access_token")))))))

(test a-confidential-client-with-the-wrong-secret-is-refused
  (with-fixture state ()
    (register-oauth-client :client-id "conf-client" :public nil
                           :redirect-uris (list "http://127.0.0.1/callback"))
    (cl-mock:with-mocks ()
      (cl-mock:if-called 'hunchentoot:authorization
                         (lambda () (values "conf-client" "not-the-secret")))
      (is (equal "invalid_client"
                 (field (post-token "grant_type" "authorization_code")
                        "error"))))))

(test an-unsupported-grant-type-is-named-in-the-error
  (with-fixture state ()
    (is (equal "unsupported_grant_type"
               (field (post-token "grant_type" "password"
                                  "client_id" "test-client")
                      "error")))
    (is (equal "unsupported_grant_type"
               (field (post-token "client_id" "test-client")
                      "error")))))

;; ----------------------------------------------------------------------
;; grant_type=refresh_token
;; ----------------------------------------------------------------------

(test refreshing-rotates-the-refresh-token
  (with-fixture state ()
    (let* ((grant (make-grant))
           (refresh-token (make-refresh-token grant))
           (response (post-token "grant_type" "refresh_token"
                                 "client_id" "test-client"
                                 "refresh_token" (refresh-token-string refresh-token))))
      (is-true (field response "access_token"))
      (is-true (field response "refresh_token"))
      ;; A new one, and the old one is dead.
      (is-false (equal (refresh-token-string refresh-token)
                       (field response "refresh_token")))
      (is-true (refresh-token-revoked-p refresh-token)))))

(test reusing-a-rotated-refresh-token-revokes-the-grant
  "RFC 6819 §5.2.2.3: with rotation, a replayed refresh token means a copy
of it is out there."
  (with-fixture state ()
    (let* ((grant (make-grant))
           (refresh-token (make-refresh-token grant))
           (token-string (refresh-token-string refresh-token)))
      (post-token "grant_type" "refresh_token"
                  "client_id" "test-client"
                  "refresh_token" token-string)
      (is-false (grant-revoked-p grant))
      (let ((replay (post-token "grant_type" "refresh_token"
                                "client_id" "test-client"
                                "refresh_token" token-string)))
        (is (equal "invalid_grant" (field replay "error"))))
      (is-true (grant-revoked-p grant)))))

(test refreshing-can-narrow-the-scope-but-not-widen-it
  (with-fixture state ()
    (let* ((grant (make-grant :scopes '("api:read" "api:write")))
           (refresh-token (make-refresh-token grant)))
      (let ((narrowed (post-token "grant_type" "refresh_token"
                                  "client_id" "test-client"
                                  "refresh_token" (refresh-token-string refresh-token)
                                  "scope" "api:read")))
        (is (equal "api:read" (field narrowed "scope")))
        ;; Now try to climb back up using the rotated token.
        (let ((widened (post-token "grant_type" "refresh_token"
                                   "client_id" "test-client"
                                   "refresh_token" (field narrowed "refresh_token")
                                   "scope" "api:read api:write profile")))
          (is (equal "invalid_scope" (field widened "error"))))))))

(test refreshing-without-a-scope-keeps-the-original-scopes
  (with-fixture state ()
    (let* ((grant (make-grant :scopes '("api:read" "api:write")))
           (refresh-token (make-refresh-token grant)))
      (is (equal "api:read api:write"
                 (field (post-token "grant_type" "refresh_token"
                                    "client_id" "test-client"
                                    "refresh_token" (refresh-token-string refresh-token))
                        "scope"))))))

(test an-expired-refresh-token-is-rejected
  (with-fixture state ()
    (let ((refresh-token (let ((*refresh-token-ttl* -1))
                           (make-refresh-token (make-grant)))))
      (is (equal "invalid_grant"
                 (field (post-token "grant_type" "refresh_token"
                                    "client_id" "test-client"
                                    "refresh_token" (refresh-token-string refresh-token))
                        "error"))))))

(test an-unknown-refresh-token-is-rejected
  (with-fixture state ()
    (is (equal "invalid_grant"
               (field (post-token "grant_type" "refresh_token"
                                  "client_id" "test-client"
                                  "refresh_token" "nope")
                      "error")))))

(test a-refresh-token-cannot-be-used-by-another-client
  (with-fixture state ()
    (register-oauth-client :client-id "other-client")
    (let ((refresh-token (make-refresh-token (make-grant))))
      (is (equal "invalid_grant"
                 (field (post-token "grant_type" "refresh_token"
                                    "client_id" "other-client"
                                    "refresh_token" (refresh-token-string refresh-token))
                        "error"))))))

;; ----------------------------------------------------------------------
;; grant_type=device_code
;; ----------------------------------------------------------------------

(defun poll-device (device-code &key (client-id "test-client"))
  (post-token "grant_type" +device-code-grant-type+
              "client_id" client-id
              "device_code" device-code))

(test polling-a-pending-device-request-says-authorization-pending
  (with-fixture state ()
    (let ((request (make-device-request :client client :scopes '("api:read"))))
      (is (equal "authorization_pending"
                 (field (poll-device (device-code-string request)) "error"))))))

(test polling-too-fast-says-slow-down
  (with-fixture state ()
    (let ((request (make-device-request :client client)))
      (poll-device (device-code-string request))
      (is (equal "slow_down"
                 (field (poll-device (device-code-string request)) "error"))))))

(test an-approved-device-request-yields-tokens-exactly-once
  (with-fixture state ()
    (let ((request (make-device-request :client client :scopes '("api:read")))
          (grant (make-grant)))
      (approve-device-request request grant)
      (let ((response (poll-device (device-code-string request))))
        (is (equal "Bearer" (field response "token_type")))
        (is-true (field response "access_token"))
        (is-true (field response "refresh_token")))
      (is (eql :consumed (device-status request)))
      ;; A second exchange of the same device code fails.
      (is (equal "invalid_grant"
                 (field (poll-device (device-code-string request)) "error"))))))

(test a-denied-device-request-says-access-denied
  (with-fixture state ()
    (let ((request (make-device-request :client client)))
      (deny-device-request request)
      (is (equal "access_denied"
                 (field (poll-device (device-code-string request)) "error"))))))

(test an-expired-device-code-says-expired-token
  (with-fixture state ()
    (let ((request (let ((*device-code-ttl* -1))
                     (make-device-request :client client))))
      (is (equal "expired_token"
                 (field (poll-device (device-code-string request)) "error"))))))

(test a-device-code-cannot-be-polled-by-another-client
  (with-fixture state ()
    (register-oauth-client :client-id "other-client")
    (let ((request (make-device-request :client client)))
      (is (equal "invalid_grant"
                 (field (poll-device (device-code-string request)
                                     :client-id "other-client")
                        "error"))))))

(test an-unknown-device-code-is-rejected
  (with-fixture state ()
    (is (equal "invalid_grant"
               (field (poll-device "nope") "error")))))

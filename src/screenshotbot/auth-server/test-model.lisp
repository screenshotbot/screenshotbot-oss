;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-model
  (:use #:cl
        #:fiveam)
  (:import-from #:core/api/model/api-key
                #:api-key-permissions
                #:api-key-user-visible-p
                #:company-api-keys
                #:%find-api-key
                #:api-key
                #:decode-api-token
                #:validate-api-key-secret)
  (:import-from #:screenshotbot/auth-server/model
                #:*access-token-ttl*
                #:+cli-client-id+
                #:access-token-expires-in
                #:access-token-string
                #:approve-device-request
                #:*authorization-code-ttl*
                #:*device-code-ttl*
                #:cleanup-expired-oauth-objects
                #:code-consumed-p
                #:code-string
                #:consume-device-request
                #:consume-oauth-code
                #:device-code-string
                #:device-status
                #:device-user-code
                #:ensure-builtin-clients
                #:find-device-request
                #:find-device-request-by-user-code
                #:find-oauth-client
                #:find-oauth-code
                #:grant-valid-p
                #:make-access-token
                #:make-device-request
                #:make-oauth-code
                #:normalize-user-code
                #:note-device-poll
                #:oauth-client
                #:oauth-client-id
                #:oauth-client-secret
                #:oauth-grant
                #:public-client-p
                #:redirect-uri-allowed-p
                #:register-oauth-client
                #:revoke-grant)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/auth-server/test-model)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:company company :user user)
      (let ((client (register-oauth-client
                     :client-id "test-client"
                     :name "Test Client"
                     :redirect-uris (list "http://127.0.0.1/callback"
                                          "https://app.example.com/cb")
                     :scopes (list "profile" "api:read" "api:write"))))
        (flet ((make-grant (&key (scopes '("api:read")))
                 (make-instance 'oauth-grant
                                :client client
                                :user user
                                :company company
                                :scopes scopes)))
          (&body))))))

;; ----------------------------------------------------------------------
;; Clients
;; ----------------------------------------------------------------------

(test clients-are-public-unless-given-a-secret
  (with-fixture state ()
    (is-true (public-client-p client))
    (is-true (public-client-p (register-oauth-client :client-id "pub")))
    (let ((confidential (register-oauth-client :client-id "conf" :public nil)))
      (is-false (public-client-p confidential))
      (is-true (> (length (oauth-client-secret confidential)) 20)))))

(test the-builtin-cli-client-is-created-once
  (with-fixture state ()
    (let ((first (ensure-builtin-clients)))
      (is-true first)
      (is (eq first (ensure-builtin-clients)))
      (is (eq first (find-oauth-client +cli-client-id+)))
      (is-true (public-client-p first)))))

(test exact-redirect-uris-must-match-exactly
  (with-fixture state ()
    (is-true (redirect-uri-allowed-p client "https://app.example.com/cb"))
    (is-false (redirect-uri-allowed-p client "https://app.example.com/cb2"))
    (is-false (redirect-uri-allowed-p client "https://evil.example.com/cb"))
    ;; An open redirect via a query suffix would be just as bad.
    (is-false (redirect-uri-allowed-p client "https://app.example.com/cb/../x"))
    (is-false (redirect-uri-allowed-p client ""))
    (is-false (redirect-uri-allowed-p client "not a uri at all"))))

(test loopback-redirects-ignore-the-port
  "RFC 8252 §7.3: a CLI can't reserve a port ahead of time."
  (with-fixture state ()
    (is-true (redirect-uri-allowed-p client "http://127.0.0.1:1234/callback"))
    (is-true (redirect-uri-allowed-p client "http://127.0.0.1:65535/callback"))
    (is-true (redirect-uri-allowed-p client "http://127.0.0.1/callback"))
    ;; ...but only the port is flexible.
    (is-false (redirect-uri-allowed-p client "http://127.0.0.1:1234/other"))
    (is-false (redirect-uri-allowed-p client "https://127.0.0.1:1234/callback"))
    (is-false (redirect-uri-allowed-p client "http://127.0.0.2:1234/callback"))
    ;; localhost was not registered for this client.
    (is-false (redirect-uri-allowed-p client "http://localhost:1234/callback"))))

(test a-non-loopback-host-does-not-borrow-loopback-leniency
  (with-fixture state ()
    (let ((c (register-oauth-client :client-id "c2"
                                    :redirect-uris (list "http://example.com/cb"))))
      (is-true (redirect-uri-allowed-p c "http://example.com/cb"))
      (is-false (redirect-uri-allowed-p c "http://example.com:8080/cb")))))

;; ----------------------------------------------------------------------
;; Authorization codes
;; ----------------------------------------------------------------------

(test authorization-codes-are-single-use
  (with-fixture state ()
    (let ((code (make-oauth-code :grant (make-grant)
                                 :redirect-uri "http://127.0.0.1:1/callback")))
      (is-false (code-consumed-p code))
      (is-true (consume-oauth-code code))
      (is-true (code-consumed-p code))
      ;; Every later attempt loses.
      (is-false (consume-oauth-code code))
      (is-false (consume-oauth-code code)))))

(test authorization-codes-are-findable-and-unguessable
  (with-fixture state ()
    (let* ((code (make-oauth-code :grant (make-grant)))
           (other (make-oauth-code :grant (make-grant))))
      (is (eq code (find-oauth-code
                    (code-string code))))
      (is-false (equal (code-string code)
                       (code-string other)))
      (is (>= (length (code-string code)) 40))
      (is-false (find-oauth-code "nope")))))

;; ----------------------------------------------------------------------
;; Grants and access tokens
;; ----------------------------------------------------------------------

(test access-tokens-authenticate-like-api-keys
  "The whole point of subclassing API-KEY: the existing API auth path
finds and validates an OAuth token with no OAuth-specific code."
  (with-fixture state ()
    (let* ((grant (make-grant))
           (token (make-access-token grant))
           (token-string (access-token-string token)))
      (multiple-value-bind (key secret) (decode-api-token token-string)
        (declare (ignore secret))
        (is (eq token (%find-api-key key)))
        (is-true (validate-api-key-secret token token-string))))))

(test access-token-permissions-come-from-the-grant-scopes
  (with-fixture state ()
    (is (equal '(:full)
               (api-key-permissions (make-access-token
                                     (make-grant :scopes '("api:read"))))))
    (is (equal nil
               (api-key-permissions (make-access-token
                                     (make-grant :scopes '("profile"))))))))

(test a-refresh-can-narrow-but-the-grant-is-unchanged
  (with-fixture state ()
    (let ((grant (make-grant :scopes '("api:read" "api:write"))))
      (is (equal '(:full)
                 (api-key-permissions
                  (make-access-token grant :scopes '("api:read"))))))))

(test revoking-a-grant-invalidates-its-access-tokens
  (with-fixture state ()
    (let* ((grant (make-grant))
           (token (make-access-token grant)))
      (multiple-value-bind (key) (decode-api-token (access-token-string token))
        (is (eq token (%find-api-key key)))
        (revoke-grant grant)
        (is-false (grant-valid-p grant))
        ;; %FIND-API-KEY filters expired keys, so the token stops
        ;; authenticating immediately rather than at its expiry.
        (is-false (%find-api-key key))))))

(test expired-access-tokens-stop-authenticating
  (with-fixture state ()
    (let ((*access-token-ttl* -1))
      (let ((token (make-access-token (make-grant))))
        (is (equal 0 (access-token-expires-in token)))
        (multiple-value-bind (key) (decode-api-token (access-token-string token))
          (is-false (%find-api-key key)))))))

(test access-tokens-are-hidden-from-the-api-keys-dashboard
  (with-fixture state ()
    (let ((token (make-access-token (make-grant)))
          (plain (make-instance 'api-key :user user :company company)))
      (is-false (api-key-user-visible-p token))
      (is-true (api-key-user-visible-p plain))
      (let ((listed (company-api-keys company)))
        (is-true (member plain listed))
        (is-false (member token listed))))))

;; ----------------------------------------------------------------------
;; Device flow
;; ----------------------------------------------------------------------

(test device-user-codes-are-readable-and-unambiguous
  (with-fixture state ()
    (loop repeat 25
          for request = (make-device-request :client client :scopes '("profile"))
          for code = (device-user-code request)
          do
             (is (equal 9 (length code)))
             (is (equal #\- (aref code 4)))
             ;; No vowels, so a code can never come out as a real word.
             (is-false (some (lambda (ch) (find ch "AEIOU")) code))
             ;; No digits either, so there is no 0/O or 1/I to misread.
             (is-false (some #'digit-char-p code)))))

(test normalize-user-code-accepts-what-a-human-would-type
  (with-fixture state ()
    (let* ((request (make-device-request :client client))
           (code (device-user-code request)))
      (is (eq request (find-device-request-by-user-code
                       (normalize-user-code code))))
      (is (eq request (find-device-request-by-user-code
                       (normalize-user-code (string-downcase code)))))
      (is (eq request (find-device-request-by-user-code
                       (normalize-user-code (remove #\- code)))))
      (is (eq request (find-device-request-by-user-code
                       (normalize-user-code (format nil "  ~a  " code))))))))

(test device-requests-start-pending-and-are-consumed-once
  (with-fixture state ()
    (let ((request (make-device-request :client client :scopes '("api:read")))
          (grant (make-grant)))
      (is (eql :pending (device-status request)))
      (is-false (consume-device-request request))
      (approve-device-request request grant)
      (is (eql :approved (device-status request)))
      (is (eq grant (consume-device-request request)))
      (is (eql :consumed (device-status request)))
      ;; RFC 8628 §3.5: a device code is good for exactly one exchange.
      (is-false (consume-device-request request)))))

(test note-device-poll-detects-polling-that-is-too-fast
  (with-fixture state ()
    (let ((request (make-device-request :client client)))
      ;; The first poll has nothing to compare against.
      (is-false (note-device-poll request))
      ;; The second poll is immediate, and the interval is 5s.
      (is-true (note-device-poll request)))))

;; ----------------------------------------------------------------------
;; Expiry
;; ----------------------------------------------------------------------

(test cleanup-removes-expired-objects-and-keeps-live-ones
  (with-fixture state ()
    (let ((live (make-oauth-code :grant (make-grant)))
          (dead (let ((*authorization-code-ttl* -10))
                  (make-oauth-code :grant (make-grant))))
          (dead-device (let ((*device-code-ttl* -10))
                         (make-device-request :client client))))
      (let ((dead-code-string (code-string dead))
            (dead-device-string (device-code-string dead-device)))
        (cleanup-expired-oauth-objects)
        (is-false (find-oauth-code dead-code-string))
        (is-false (find-device-request dead-device-string))
        (is (eq live (find-oauth-code
                      (code-string live))))))))

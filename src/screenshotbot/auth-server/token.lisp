;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/token
  (:use #:cl)
  (:import-from #:screenshotbot/auth-server/errors
                #:oauth-error!
                #:with-oauth-json-errors
                #:write-json)
  (:import-from #:screenshotbot/auth-server/model
                #:access-token-expires-in
                #:access-token-string
                #:code-challenge
                #:code-challenge-method
                #:code-expires-at
                #:code-grant
                #:code-redirect-uri
                #:code-resource
                #:device-resource
                #:refresh-token-resource
                #:consume-device-request
                #:consume-oauth-code
                #:device-client
                #:device-expires-at
                #:device-status
                #:ensure-builtin-clients
                #:find-device-request
                #:find-oauth-client
                #:find-oauth-code
                #:find-refresh-token
                #:grant-client
                #:grant-scopes
                #:grant-valid-p
                #:make-access-token
                #:make-refresh-token
                #:note-device-poll
                #:oauth-client-secret
                #:public-client-p
                #:refresh-token-expires-at
                #:refresh-token-grant
                #:refresh-token-revoked-p
                #:refresh-token-string
                #:revoke-grant
                #:revoke-refresh-token)
  (:import-from #:screenshotbot/auth-server/pkce
                #:constant-time-equal
                #:verify-code-verifier)
  (:import-from #:screenshotbot/auth-server/resource-indicators
                #:narrowed-resource
                #:read-resource)
  (:import-from #:screenshotbot/auth-server/scopes
                #:parse-scope-string
                #:render-scope-list)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util/throttler
                #:throttle!
                #:throttler)
  (:export
   #:+device-code-grant-type+
   #:authenticate-client
   #:supported-grant-types))
(in-package :screenshotbot/auth-server/token)

(defparameter +device-code-grant-type+
  "urn:ietf:params:oauth:grant-type:device_code"
  "RFC 8628 §3.4.")

(defun supported-grant-types ()
  (list "authorization_code" "refresh_token" +device-code-grant-type+))

(defvar *throttler* (make-instance 'throttler :tokens 600)
  "The token endpoint is unauthenticated until the credentials are checked,
so it needs its own rate limit.")

;; ----------------------------------------------------------------------
;; Client authentication
;; ----------------------------------------------------------------------

(defun authenticate-client (&key client-id)
  "Identify the client making a token request.

Confidential clients authenticate with HTTP Basic (RFC 6749 §2.3.1);
public clients -- every CLI -- just send client_id in the body, which
identifies but does not authenticate them. That is why PKCE and the
per-code redirect_uri check carry the weight for public clients."
  (multiple-value-bind (basic-id basic-secret) (hunchentoot:authorization)
    (let* ((client-id (or basic-id client-id))
           (client (unless (str:emptyp client-id)
                     (find-oauth-client client-id))))
      (unless client
        (oauth-error! "invalid_client" "Unknown or missing client_id" :status 401))
      (unless (or
               (public-client-p client)
               (constant-time-equal (oauth-client-secret client) basic-secret))
        (oauth-error! "invalid_client" "Invalid client credentials" :status 401))
      client)))

(defun %check-client (client grant what)
  (unless (eq client (grant-client grant))
    (oauth-error! "invalid_grant"
                  (format nil "This ~a was issued to a different client" what))))

;; ----------------------------------------------------------------------
;; Responses
;; ----------------------------------------------------------------------

(defun %token-response (grant &key scopes refresh-token resource)
  (let* ((scopes (or scopes (grant-scopes grant)))
         (access-token (make-access-token grant :scopes scopes
                                                :resource resource)))
    `(("access_token" . ,(access-token-string access-token))
      ("token_type" . "Bearer")
      ("expires_in" . ,(access-token-expires-in access-token))
      ,@(when refresh-token
          `(("refresh_token" . ,(refresh-token-string refresh-token))))
      ("scope" . ,(render-scope-list scopes)))))

(defun %requested-resource ()
  "The RFC 8707 resource indicator on this token request, if any."
  (read-resource (hunchentoot:post-parameters*)))

;; ----------------------------------------------------------------------
;; grant_type=authorization_code
;; ----------------------------------------------------------------------

(defun %authorization-code-grant (client)
  (let ((code-string (hunchentoot:post-parameter "code"))
        (redirect-uri (hunchentoot:post-parameter "redirect_uri"))
        (code-verifier (hunchentoot:post-parameter "code_verifier")))
    (when (str:emptyp code-string)
      (oauth-error! "invalid_request" "code is required"))
    (let ((code (find-oauth-code code-string)))
      (unless code
        (oauth-error! "invalid_grant" "Unknown or expired authorization code"))
      (let ((grant (code-grant code)))
        (%check-client client grant "authorization code")
        (when (< (code-expires-at code) (get-universal-time))
          (oauth-error! "invalid_grant" "The authorization code has expired"))

        ;; Consume before checking the verifier, so that a wrong
        ;; code_verifier costs an attacker the code rather than giving
        ;; them unlimited guesses.
        (unless (consume-oauth-code code)
          ;; RFC 6819 §5.2.1.1: a replayed code means it leaked, and we
          ;; can't tell which of the two callers was the attacker.
          (revoke-grant grant)
          (oauth-error! "invalid_grant" "This authorization code has already been used"))

        (unless (grant-valid-p grant)
          (oauth-error! "invalid_grant" "This authorization has been revoked"))
        (unless (equal redirect-uri (code-redirect-uri code))
          (oauth-error! "invalid_grant"
                        "redirect_uri does not match the authorization request"))
        (when (code-challenge code)
          (unless (verify-code-verifier
                   :code-challenge (code-challenge code)
                   :code-challenge-method (code-challenge-method code)
                   :code-verifier code-verifier)
            (oauth-error! "invalid_grant" "PKCE verification failed")))

        ;; RFC 8707 §2.2: the token request may repeat the resource, and it
        ;; has to be the one the code was authorized for. Omitting it
        ;; inherits, so a client can name the resource once at the
        ;; authorization endpoint and not again here.
        (let ((resource (narrowed-resource (%requested-resource)
                                           (code-resource code))))
          (%token-response grant
                           :resource resource
                           :refresh-token (make-refresh-token
                                           grant :resource resource)))))))

;; ----------------------------------------------------------------------
;; grant_type=refresh_token
;; ----------------------------------------------------------------------

(defun %narrowed-scopes (grant scope)
  "RFC 6749 §6: a refresh may ask for a subset of what was granted, never
more. An absent scope means the original set."
  (cond
    ((str:emptyp scope)
     (grant-scopes grant))
    (t
     (multiple-value-bind (known unknown) (parse-scope-string scope)
       (let ((extra (append unknown
                            (set-difference known (grant-scopes grant)
                                            :test #'equal))))
         (when extra
           (oauth-error! "invalid_scope"
                         (format nil "Scope(s) not covered by this authorization: ~a"
                                 (str:join ", " extra)))))
       known))))

(defun %refresh-token-grant (client)
  (let ((token-string (hunchentoot:post-parameter "refresh_token"))
        (scope (hunchentoot:post-parameter "scope")))
    (when (str:emptyp token-string)
      (oauth-error! "invalid_request" "refresh_token is required"))
    (let ((refresh-token (find-refresh-token token-string)))
      (unless refresh-token
        (oauth-error! "invalid_grant" "Unknown or expired refresh token"))
      (let ((grant (refresh-token-grant refresh-token)))
        (%check-client client grant "refresh token")
        (when (refresh-token-revoked-p refresh-token)
          ;; We rotate on every use, so a revoked token being presented
          ;; means someone kept a copy. RFC 6819 §5.2.2.3.
          (revoke-grant grant)
          (oauth-error! "invalid_grant" "This refresh token has already been used"))
        (when (< (refresh-token-expires-at refresh-token) (get-universal-time))
          (oauth-error! "invalid_grant" "The refresh token has expired"))
        (unless (grant-valid-p grant)
          (oauth-error! "invalid_grant" "This authorization has been revoked"))

        (let ((scopes (%narrowed-scopes grant scope))
              ;; The audience rides along across rotation. A refresh can
              ;; never reach a resource the original exchange didn't.
              (resource (narrowed-resource (%requested-resource)
                                           (refresh-token-resource refresh-token))))
          (revoke-refresh-token refresh-token)
          (%token-response grant
                           :scopes scopes
                           :resource resource
                           :refresh-token (make-refresh-token
                                           grant :resource resource)))))))

;; ----------------------------------------------------------------------
;; grant_type=urn:ietf:params:oauth:grant-type:device_code
;; ----------------------------------------------------------------------

(defun %device-code-grant (client)
  (let ((device-code (hunchentoot:post-parameter "device_code")))
    (when (str:emptyp device-code)
      (oauth-error! "invalid_request" "device_code is required"))
    (let ((request (find-device-request device-code)))
      (unless request
        (oauth-error! "invalid_grant" "Unknown or expired device code"))
      (unless (eq client (device-client request))
        (oauth-error! "invalid_grant" "This device code was issued to a different client"))
      (when (< (device-expires-at request) (get-universal-time))
        (oauth-error! "expired_token" "The device code has expired, start over"))

      (ecase (device-status request)
        (:denied
         (oauth-error! "access_denied" "The user denied the request"))
        (:consumed
         (oauth-error! "invalid_grant" "This device code has already been used"))
        (:pending
         ;; RFC 8628 §3.5 makes slow_down a variant of
         ;; authorization_pending, so it only applies while we're still
         ;; waiting. Returning it for a terminal state would tell the
         ;; client to back off and keep polling something that will never
         ;; succeed.
         (cond
           ((note-device-poll request)
            (oauth-error! "slow_down" "Polling faster than the interval you were given"))
           (t
            (oauth-error! "authorization_pending"
                          "The user has not approved this request yet"))))
        (:approved
         (let ((grant (consume-device-request request)))
           (unless grant
             (oauth-error! "invalid_grant" "This device code has already been used"))
           (unless (grant-valid-p grant)
             (oauth-error! "invalid_grant" "This authorization has been revoked"))
           (let ((resource (narrowed-resource (%requested-resource)
                                              (device-resource request))))
             (%token-response grant
                              :resource resource
                              :refresh-token (make-refresh-token
                                              grant :resource resource)))))))))

;; ----------------------------------------------------------------------
;; The endpoint
;; ----------------------------------------------------------------------

(defun %token ()
  (ensure-builtin-clients)
  (throttle! *throttler* :key (hunchentoot:real-remote-addr))
  (let ((client (authenticate-client
                 :client-id (hunchentoot:post-parameter "client_id")))
        (grant-type (hunchentoot:post-parameter "grant_type")))
    (write-json
     (cond
       ((equal "authorization_code" grant-type)
        (%authorization-code-grant client))
       ((equal "refresh_token" grant-type)
        (%refresh-token-grant client))
       ((equal +device-code-grant-type+ grant-type)
        (%device-code-grant client))
       (t
        (oauth-error! "unsupported_grant_type"
                      (format nil "Unsupported grant_type: ~a"
                              (or grant-type "(none)"))))))))

(defhandler (nil :uri "/oauth/token" :method :post) ()
  (with-oauth-json-errors ()
    (%token)))

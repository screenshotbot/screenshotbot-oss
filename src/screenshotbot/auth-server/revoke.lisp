;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/revoke
  (:use #:cl)
  (:import-from #:core/api/model/api-key
                #:%find-api-key
                #:decode-api-token)
  (:import-from #:screenshotbot/auth-server/errors
                #:oauth-error!
                #:with-oauth-json-errors)
  (:import-from #:screenshotbot/auth-server/model
                #:access-token-grant
                #:ensure-builtin-clients
                #:find-refresh-token
                #:grant-client
                #:oauth-access-token
                #:refresh-token-grant
                #:revoke-grant)
  (:import-from #:screenshotbot/auth-server/token
                #:authenticate-client)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util/throttler
                #:throttle!
                #:throttler)
  (:export
   #:grant-for-token)
  (:documentation "OAuth 2.0 Token Revocation, RFC 7009."))
(in-package :screenshotbot/auth-server/revoke)

(defvar *throttler* (make-instance 'throttler :tokens 300))

(defun grant-for-token (token)
  "The grant behind TOKEN, whether it's a refresh token or an access token.

RFC 7009 §2.1 lets us accept either, and says the token_type_hint is only
a hint, so we just try both."
  (or
   (let ((refresh-token (find-refresh-token token)))
     (when refresh-token
       (refresh-token-grant refresh-token)))
   (let ((access-token (ignore-errors
                        (multiple-value-bind (key) (decode-api-token token)
                          (%find-api-key key)))))
     (when (typep access-token 'oauth-access-token)
       (access-token-grant access-token)))))

(defun %revoke ()
  (ensure-builtin-clients)
  (throttle! *throttler* :key (hunchentoot:real-remote-addr))
  (let ((client (authenticate-client
                 :client-id (hunchentoot:post-parameter "client_id")))
        (token (hunchentoot:post-parameter "token")))
    (when (str:emptyp token)
      (oauth-error! "invalid_request" "token is required"))
    (let ((grant (grant-for-token token)))
      ;; RFC 7009 §2.2: an unknown token is not an error, so that a client
      ;; cleaning up can't use this endpoint to probe which tokens exist.
      ;; A token belonging to someone else is silently ignored for the
      ;; same reason.
      (when (and grant (eq client (grant-client grant)))
        ;; §2.1 says revoking an access token SHOULD take the refresh
        ;; token with it. Revoking the whole grant does that, and is what
        ;; a user pressing "disconnect" expects either way.
        (revoke-grant grant)))
    (setf (hunchentoot:return-code*) 200)
    (setf (hunchentoot:header-out :cache-control) "no-store")
    ""))

(defhandler (nil :uri "/oauth/revoke" :method :post) ()
  (with-oauth-json-errors ()
    (%revoke)))

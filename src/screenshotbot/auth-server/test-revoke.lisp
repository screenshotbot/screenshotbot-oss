;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-revoke
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
                #:access-token-string
                #:grant-revoked-p
                #:make-access-token
                #:make-refresh-token
                #:oauth-grant
                #:refresh-token-string
                #:register-oauth-client)
  (:import-from #:screenshotbot/auth-server/revoke
                #:%revoke
                #:grant-for-token)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:documentation "Tests for token revocation, RFC 7009."))
(in-package :screenshotbot/auth-server/test-revoke)

(util/fiveam:def-suite)

(defun post-revoke (&rest params)
  "Returns the HTTP status, since a successful revocation has no body."
  (with-fake-request (:script-name "/oauth/revoke")
    (loop for (key value) on params by #'cddr
          if value
            do (setf (hunchentoot:post-parameter hunchentoot:*request* key) value))
    (let ((body (with-oauth-json-errors ()
                  (%revoke))))
      (values (hunchentoot:return-code*) body))))

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:company company :user user)
      (let ((client (register-oauth-client
                     :client-id "test-client"
                     :scopes (list "profile" "api:read"))))
        (flet ((make-grant ()
                 (make-instance 'oauth-grant
                                :client client
                                :user user
                                :company company
                                :scopes '("api:read"))))
          (&body))))))

(test revoking-a-refresh-token-kills-the-whole-grant
  (with-fixture state ()
    (let* ((grant (make-grant))
           (refresh-token (make-refresh-token grant)))
      (is (equal 200 (post-revoke "client_id" "test-client"
                                  "token" (refresh-token-string refresh-token))))
      (is-true (grant-revoked-p grant)))))

(test revoking-an-access-token-also-kills-the-grant
  "RFC 7009 §2.1 says revoking an access token SHOULD take the refresh
token with it, and it is what a user pressing `disconnect' expects."
  (with-fixture state ()
    (let* ((grant (make-grant))
           (access-token (make-access-token grant)))
      (is (equal 200 (post-revoke "client_id" "test-client"
                                  "token" (access-token-string access-token))))
      (is-true (grant-revoked-p grant))
      ;; And it stops authenticating immediately.
      (multiple-value-bind (key) (decode-api-token (access-token-string access-token))
        (is-false (%find-api-key key))))))

(test an-unknown-token-is-accepted-in-silence
  "RFC 7009 §2.2: erroring would turn this endpoint into an oracle for
which tokens exist."
  (with-fixture state ()
    (is (equal 200 (post-revoke "client_id" "test-client" "token" "no-such-token")))))

(test another-clients-token-is-ignored-not-revoked
  (with-fixture state ()
    (let* ((grant (make-grant))
           (refresh-token (make-refresh-token grant)))
      (register-oauth-client :client-id "other-client")
      ;; Silently, so this can't be used to probe for valid tokens either.
      (is (equal 200 (post-revoke "client_id" "other-client"
                                  "token" (refresh-token-string refresh-token))))
      (is-false (grant-revoked-p grant)))))

(test revocation-still-requires-a-known-client
  (with-fixture state ()
    (let* ((grant (make-grant))
           (refresh-token (make-refresh-token grant)))
      (with-fake-request (:script-name "/oauth/revoke")
        (setf (hunchentoot:post-parameter hunchentoot:*request* "client_id") "nope")
        (setf (hunchentoot:post-parameter hunchentoot:*request* "token")
              (refresh-token-string refresh-token))
        (let ((response (with-oauth-json-errors () (%revoke))))
          (is-true (str:containsp "invalid_client" response))))
      (is-false (grant-revoked-p grant)))))

(test a-missing-token-is-a-bad-request
  (with-fixture state ()
    (with-fake-request (:script-name "/oauth/revoke")
      (setf (hunchentoot:post-parameter hunchentoot:*request* "client_id") "test-client")
      (let ((response (with-oauth-json-errors () (%revoke))))
        (is-true (str:containsp "invalid_request" response))))))

(test grant-for-token-recognises-both-token-kinds
  (with-fixture state ()
    (let* ((grant (make-grant))
           (refresh-token (make-refresh-token grant))
           (access-token (make-access-token grant)))
      (is (eq grant (grant-for-token (refresh-token-string refresh-token))))
      (is (eq grant (grant-for-token (access-token-string access-token))))
      (is-false (grant-for-token "garbage"))
      (is-false (grant-for-token "")))))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/test-client
  (:use #:cl
        #:fiveam)
  (:import-from #:auth-server/client
                #:access-token
                #:oauth-client
                #:sign-out)
  (:import-from #:auth-server/client/conditions
                #:oauth-error)
  (:import-from #:auth-server/client/flows
                #:authorization-code-flow
                #:device-flow
                #:refresh-tokens
                #:revoke-token)
  (:import-from #:auth-server/client/token-store
                #:access-token-string
                #:load-tokens
                #:refresh-token-string
                #:save-tokens)
  (:import-from #:cl-mock
                #:if-called
                #:with-mocks)
  (:documentation "Tests for the one decision the client makes on every call:
reuse the cached token, refresh it, or sign in again.

Every flow is stubbed, so nothing here touches a socket or a browser --
the point is which branch gets taken, and what ends up on disk."))
(in-package :auth-server/client/test-client)

(util/fiveam:def-suite)

(defparameter +host+ "https://staging.screenshotbot.io")

(defun token-response (&key (access "fresh-access") (refresh "fresh-refresh")
                         (expires-in 3600))
  `(("access_token" . ,access)
    ("refresh_token" . ,refresh)
    ("token_type" . "Bearer")
    ("expires_in" . ,expires-in)
    ("scope" . "api:read")))

(def-fixture state ()
  (with-mocks ()
    (tmpdir:with-tmpdir (dir)
      (let* ((token-file (merge-pathnames "oauth-token.json" dir))
             (signed-in 0)
             (refreshed 0)
             (revoked nil)
             (client (make-instance 'oauth-client
                                    :host +host+
                                    :client-id "cli"
                                    :scope "api:read"
                                    ;; Never let discovery reach the network.
                                    :use-discovery-p nil
                                    :token-file token-file)))
        (if-called 'authorization-code-flow
                   (lambda (metadata &key client-id scope open-browser timeout)
                     (declare (ignore metadata client-id scope open-browser timeout))
                     (incf signed-in)
                     (token-response :access "signed-in-access")))
        (if-called 'device-flow
                   (lambda (metadata &key client-id scope open-browser timeout)
                     (declare (ignore metadata client-id scope open-browser timeout))
                     (incf signed-in)
                     (token-response :access "device-access")))
        (if-called 'refresh-tokens
                   (lambda (metadata &key client-id refresh-token scope)
                     (declare (ignore metadata client-id refresh-token scope))
                     (incf refreshed)
                     (token-response :access "refreshed-access"
                                     :refresh "rotated-refresh")))
        (if-called 'revoke-token
                   (lambda (metadata &key client-id token)
                     (declare (ignore metadata client-id))
                     (setf revoked token)
                     t))
        (&body)))))

(test a-usable-cached-token-is-reused-without-touching-the-network
  (with-fixture state ()
    (save-tokens token-file +host+ (token-response :access "cached-access"))
    (is (equal "cached-access" (access-token client)))
    (is (equal 0 signed-in))
    (is (equal 0 refreshed))))

(test with-no-cache-at-all-we-sign-in
  (with-fixture state ()
    (is (equal "signed-in-access" (access-token client)))
    (is (equal 1 signed-in))
    (is (equal 0 refreshed))))

(test an-expired-token-is-refreshed-rather-than-re-authorized
  "Refreshing is invisible; signing in interrupts the user. Prefer it."
  (with-fixture state ()
    (save-tokens token-file +host+ (token-response :expires-in -1))
    (is (equal "refreshed-access" (access-token client)))
    (is (equal 1 refreshed))
    (is (equal 0 signed-in))))

(test a-refreshed-token-is-written-back-to-the-cache
  "Otherwise every call would refresh, and rotation would revoke the grant
on the second one."
  (with-fixture state ()
    (save-tokens token-file +host+ (token-response :expires-in -1))
    (access-token client)
    (let ((cached (load-tokens token-file +host+)))
      (is (equal "refreshed-access" (access-token-string cached)))
      (is (equal "rotated-refresh" (refresh-token-string cached))))))

(test a-failed-refresh-falls-back-to-signing-in
  "A grant can be revoked from the dashboard at any time; that must not be
a dead end."
  (with-fixture state ()
    (save-tokens token-file +host+ (token-response :expires-in -1))
    (if-called 'refresh-tokens
               (lambda (metadata &key client-id refresh-token scope)
                 (declare (ignore metadata client-id refresh-token scope))
                 (error 'oauth-error :code "invalid_grant"))
               :at-start t)
    (is (equal "signed-in-access" (access-token client)))
    (is (equal 1 signed-in))))

(test an-expired-token-with-no-refresh-token-signs-in
  (with-fixture state ()
    (save-tokens token-file +host+
                 (remove "refresh_token" (token-response :expires-in -1)
                         :key #'car :test #'equal))
    (is (equal "signed-in-access" (access-token client)))
    (is (equal 1 signed-in))
    (is (equal 0 refreshed))))

(test force-new-ignores-a-perfectly-good-cached-token
  (with-fixture state ()
    (save-tokens token-file +host+ (token-response :access "cached-access"))
    (is (equal "signed-in-access" (access-token client :force-new t)))
    (is (equal 1 signed-in))
    (is (equal 0 refreshed))))

(test a-token-cached-for-another-installation-is-not-used
  "The cache is one file; a staging token must never reach production."
  (with-fixture state ()
    (save-tokens token-file "https://elsewhere.example.com"
                 (token-response :access "other-host-access"))
    (is (equal "signed-in-access" (access-token client)))
    (is (equal 1 signed-in))))

(test signing-in-writes-the-tokens-to-the-cache
  (with-fixture state ()
    (access-token client)
    (let ((cached (load-tokens token-file +host+)))
      (is-true cached)
      (is (equal "signed-in-access" (access-token-string cached))))))

(test the-device-flow-is-used-when-the-client-asks-for-it
  (with-fixture state ()
    (let ((client (make-instance 'oauth-client
                                 :host +host+ :client-id "cli"
                                 :flow :device
                                 :use-discovery-p nil
                                 :token-file token-file)))
      (is (equal "device-access" (access-token client)))
      (is (equal 1 signed-in)))))

(test sign-out-revokes-at-the-server-and-then-forgets-locally
  "Deleting the file alone would leave a live refresh token on the server."
  (with-fixture state ()
    (save-tokens token-file +host+ (token-response :refresh "the-refresh"))
    (is-true (sign-out client))
    (is (equal "the-refresh" revoked))
    (is-false (load-tokens token-file +host+))))

(test sign-out-prefers-the-refresh-token-but-settles-for-the-access-token
  "Revoking the refresh token takes the whole grant with it; the access
token only gets there via the server's lookup."
  (with-fixture state ()
    (save-tokens token-file +host+
                 (remove "refresh_token" (token-response :access "only-access")
                         :key #'car :test #'equal))
    (is-true (sign-out client))
    (is (equal "only-access" revoked))))

(test signing-out-when-nothing-is-cached-is-not-an-error
  (with-fixture state ()
    (is-false (sign-out client))
    (is-false revoked)))

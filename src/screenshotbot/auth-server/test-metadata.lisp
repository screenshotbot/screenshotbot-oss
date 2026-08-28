;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-metadata
  (:use #:cl
        #:fiveam)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:core/installation/installation
                #:abstract-installation
                #:*installation*)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:screenshotbot/auth-server/metadata
                #:authorization-server-metadata)
  (:documentation "Tests for the RFC 8414 discovery document.

A client points itself at a hostname and learns everything else from
here, so a wrong value is a client that silently can't sign in."))
(in-package :screenshotbot/auth-server/test-metadata)

(util/fiveam:def-suite)

(def-fixture state (&key (domain "https://staging.screenshotbot.io"))
  (let ((*installation* (make-instance 'abstract-installation :domain domain)))
    (&body)))

(defun field (name &optional (metadata (authorization-server-metadata)))
  (assoc-value metadata name :test #'equal))

(test every-endpoint-is-absolute-and-on-the-installation-domain
  "Relative URLs here would resolve against whatever the client guessed."
  (with-fixture state ()
    (dolist (name '("authorization_endpoint" "token_endpoint"
                    "device_authorization_endpoint" "revocation_endpoint"))
      (let ((url (field name)))
        (is-true url)
        (is-true (str:starts-with-p "https://staging.screenshotbot.io/" url))))))

(test the-endpoints-are-the-paths-we-actually-serve
  (with-fixture state ()
    (is (equal "https://staging.screenshotbot.io/oauth/authorize"
               (field "authorization_endpoint")))
    (is (equal "https://staging.screenshotbot.io/oauth/token"
               (field "token_endpoint")))
    (is (equal "https://staging.screenshotbot.io/oauth/device/code"
               (field "device_authorization_endpoint")))
    (is (equal "https://staging.screenshotbot.io/oauth/revoke"
               (field "revocation_endpoint")))))

(test the-issuer-is-the-installation-domain
  (with-fixture state ()
    (is (equal "https://staging.screenshotbot.io" (field "issuer")))))

(test the-document-follows-the-domain-it-is-served-from
  (with-fixture state (:domain "http://localhost:4095")
    (is (equal "http://localhost:4095" (field "issuer")))
    (is (equal "http://localhost:4095/oauth/token" (field "token_endpoint")))))

(test only-s256-is-advertised
  "Advertising plain would invite clients to downgrade to it, and the
authorization endpoint refuses it anyway."
  (with-fixture state ()
    (is (equal '("S256") (field "code_challenge_methods_supported")))))

(test the-advertised-grants-are-the-ones-the-token-endpoint-implements
  (with-fixture state ()
    (let ((grants (field "grant_types_supported")))
      (assert-that grants (has-item "authorization_code"))
      (assert-that grants (has-item "refresh_token"))
      (assert-that grants
                   (has-item "urn:ietf:params:oauth:grant-type:device_code"))
      ;; OAuth 2.1 drops these, and we never implemented them.
      (is-false (member "implicit" grants :test #'equal))
      (is-false (member "password" grants :test #'equal)))))

(test only-the-code-response-type-is-advertised
  (with-fixture state ()
    (is (equal '("code") (field "response_types_supported")))))

(test the-advertised-scopes-are-the-ones-we-issue
  (with-fixture state ()
    (let ((scopes (field "scopes_supported")))
      (assert-that scopes (has-item "profile"))
      (assert-that scopes (has-item "api:read"))
      (assert-that scopes (has-item "api:write")))))

(test public-clients-are-advertised-as-supported
  "`none' is what a CLI uses; without it a client may assume it needs a
secret it cannot keep."
  (with-fixture state ()
    (assert-that (field "token_endpoint_auth_methods_supported")
                 (has-item "none"))))

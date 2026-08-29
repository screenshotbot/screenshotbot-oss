;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-protected-resource
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
  (:import-from #:screenshotbot/auth-server/protected-resource
                #:mcp-resource-identifier
                #:mcp-resource-metadata
                #:mcp-resource-metadata-url)
  (:documentation "Tests for the RFC 9728 protected resource document."))
(in-package :screenshotbot/auth-server/test-protected-resource)

(util/fiveam:def-suite)

(def-fixture state (&key (domain "https://staging.screenshotbot.io"))
  (let ((*installation* (make-instance 'abstract-installation :domain domain)))
    (&body)))

(defun field (name &optional (metadata (mcp-resource-metadata)))
  (assoc-value metadata name :test #'equal))

(test the-resource-identifier-is-the-mcp-endpoints-canonical-uri
  (with-fixture state ()
    (is (equal "https://staging.screenshotbot.io/mcp" (mcp-resource-identifier)))
    (is (equal (mcp-resource-identifier) (field "resource")))))

(test the-metadata-url-inserts-the-resource-path-after-the-well-known-segment
  "RFC 9728 §3.1. The bare well-known path would describe the origin, not
the MCP endpoint, so getting this wrong points clients at the wrong thing."
  (with-fixture state ()
    (is (equal
         "https://staging.screenshotbot.io/.well-known/oauth-protected-resource/mcp"
         (mcp-resource-metadata-url)))))

(test the-authorization-server-listed-is-one-a-client-can-discover
  "A client reads this, then fetches the RFC 8414 document from it, so the
value has to be the issuer that document declares -- not, say, the MCP
endpoint itself."
  (with-fixture state ()
    (let ((servers (field "authorization_servers")))
      (assert-that servers (has-item "https://staging.screenshotbot.io"))
      (is (equal (assoc-value (authorization-server-metadata) "issuer"
                              :test #'equal)
                 (first servers))))))

(test both-documents-follow-the-domain-they-are-served-from
  (with-fixture state (:domain "http://localhost:4095")
    (is (equal "http://localhost:4095/mcp" (field "resource")))
    (is (equal "http://localhost:4095/.well-known/oauth-protected-resource/mcp"
               (mcp-resource-metadata-url)))
    (is (equal '("http://localhost:4095") (field "authorization_servers")))))

(test only-the-authorization-header-is-advertised-for-bearer-tokens
  "A token in a query string ends up in access logs and Referer headers."
  (with-fixture state ()
    (is (equal '("header") (field "bearer_methods_supported")))))

(test the-scopes-advertised-are-ones-the-authorization-server-will-issue
  "Advertising a scope the authorization server rejects would send clients
into an invalid_scope loop with nothing to tell them why."
  (with-fixture state ()
    (let ((issuable (assoc-value (authorization-server-metadata)
                                 "scopes_supported" :test #'equal)))
      (dolist (scope (field "scopes_supported"))
        (assert-that issuable (has-item scope))))))

(test the-document-names-the-resource-for-a-human
  (with-fixture state ()
    (is-true (field "resource_name"))
    (is-true (str:starts-with-p "https://staging.screenshotbot.io"
                                (field "resource_documentation")))))

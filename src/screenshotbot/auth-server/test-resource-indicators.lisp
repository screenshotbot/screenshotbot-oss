;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-resource-indicators
  (:use #:cl
        #:fiveam)
  (:import-from #:core/installation/installation
                #:abstract-installation
                #:*installation*)
  (:import-from #:screenshotbot/auth-server/errors
                #:oauth-error
                #:oauth-error-code)
  (:import-from #:screenshotbot/auth-server/protected-resource
                #:known-resource-p
                #:mcp-resource-identifier)
  (:import-from #:screenshotbot/auth-server/resource-indicators
                #:narrowed-resource
                #:read-resource
                #:resource-parameters
                #:validate-resource!)
  (:documentation "Tests for RFC 8707 resource indicators.

All pure: what a valid indicator is, and what audience a token exchange
is allowed to end up with."))
(in-package :screenshotbot/auth-server/test-resource-indicators)

(util/fiveam:def-suite)

(def-fixture state (&key (domain "https://staging.screenshotbot.io"))
  (let ((*installation* (make-instance 'abstract-installation :domain domain)))
    (&body)))

(defun error-code-of (thunk)
  (handler-case (progn (funcall thunk) nil)
    (oauth-error (e) (oauth-error-code e))))

;; ----------------------------------------------------------------------
;; What counts as a valid target
;; ----------------------------------------------------------------------

(test our-own-mcp-endpoint-is-a-known-resource
  (with-fixture state ()
    (is-true (known-resource-p (mcp-resource-identifier)))
    (is (equal (mcp-resource-identifier)
               (validate-resource! (mcp-resource-identifier))))))

(test a-resource-we-do-not-serve-is-rejected
  "RFC 8707 §2 asks the server to reject an unknown target. Otherwise a
client could be handed a token audienced at a host we have nothing to do
with -- which is the confused-deputy shape this RFC exists to prevent."
  (with-fixture state ()
    (is (equal "invalid_target"
               (error-code-of (lambda ()
                                (validate-resource! "https://evil.example.com/")))))))

(test resource-matching-is-exact-not-by-prefix
  "Otherwise /mcp-evil would pass as /mcp."
  (with-fixture state ()
    (let ((mcp (mcp-resource-identifier)))
      (is-false (known-resource-p (format nil "~a-evil" mcp)))
      (is-false (known-resource-p (format nil "~a/" mcp)))
      (is-false (known-resource-p (format nil "~a/tools" mcp)))
      (is-false (known-resource-p (str:substring 0 (1- (length mcp)) mcp))))))

(test a-relative-or-malformed-resource-is-rejected
  (with-fixture state ()
    (dolist (bad '("/mcp" "mcp" "" "not a uri"))
      (is (equal "invalid_target"
                 (error-code-of (lambda () (validate-resource! bad))))
          "expected ~s to be rejected" bad))))

(test a-fragment-makes-the-audience-uncheckable
  "RFC 8707 §2: a fragment never reaches the server, so a token audienced
to one would have an audience nobody could verify."
  (with-fixture state ()
    (is (equal "invalid_target"
               (error-code-of
                (lambda ()
                  (validate-resource!
                   (format nil "~a#frag" (mcp-resource-identifier)))))))))

;; ----------------------------------------------------------------------
;; Reading it off a request
;; ----------------------------------------------------------------------

(test resource-parameters-picks-out-every-resource-entry
  (is (equal '("a" "b")
             (resource-parameters '(("client_id" . "cli")
                                    ("resource" . "a")
                                    ("scope" . "api:read")
                                    ("resource" . "b")))))
  (is (equal nil (resource-parameters '(("client_id" . "cli")))))
  ;; An empty value is the same as not asking.
  (is (equal nil (resource-parameters '(("resource" . ""))))))

(test asking-for-no-resource-is-allowed
  "An audience-less token is still valid anywhere that doesn't demand an
audience, which is what keeps existing CLI tokens working."
  (with-fixture state ()
    (is (equal nil (read-resource '())))
    (is (equal nil (read-resource '(("client_id" . "cli")))))))

(test one-resource-is-read-and-validated
  (with-fixture state ()
    (is (equal (mcp-resource-identifier)
               (read-resource `(("resource" . ,(mcp-resource-identifier))))))
    (is (equal "invalid_target"
               (error-code-of
                (lambda () (read-resource '(("resource" . "https://nope/")))))))))

(test several-resources-are-refused-rather-than-half-honoured
  "RFC 8707 allows a list, to mint one token good at several resources. We
serve one resource; silently using the first would give the client a
token it thinks is broader than it is."
  (with-fixture state ()
    (is (equal "invalid_target"
               (error-code-of
                (lambda ()
                  (read-resource `(("resource" . ,(mcp-resource-identifier))
                                   ("resource" . ,(mcp-resource-identifier))))))))))

;; ----------------------------------------------------------------------
;; What a token exchange may end up with
;; ----------------------------------------------------------------------

(test omitting-the-resource-inherits-what-was-authorized
  "So a client can name the resource once, at the authorization endpoint."
  (is (equal "https://x/mcp" (narrowed-resource nil "https://x/mcp")))
  (is (equal nil (narrowed-resource nil nil))))

(test repeating-the-same-resource-is-accepted
  (is (equal "https://x/mcp"
             (narrowed-resource "https://x/mcp" "https://x/mcp"))))

(test a-token-exchange-cannot-reach-a-resource-it-was-not-authorized-for
  "The whole point: an exchange can only ever produce something no more
powerful than what the user approved."
  (is (equal "invalid_target"
             (error-code-of
              (lambda () (narrowed-resource "https://x/other" "https://x/mcp")))))
  ;; Including climbing out of an unaudienced grant.
  (is (equal "invalid_target"
             (error-code-of
              (lambda () (narrowed-resource "https://x/mcp" nil))))))

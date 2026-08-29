;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-cors
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/auth-server/cors
                #:+allowed-request-headers+
                #:allow-cross-origin
                #:preflight)
  (:import-from #:util/testing
                #:with-fake-request)
  (:documentation "Tests for the cross-origin headers browser MCP clients need."))
(in-package :screenshotbot/auth-server/test-cors)

(util/fiveam:def-suite)

(defun header (name)
  (hunchentoot:header-out name))

(test any-origin-may-read-these-responses
  (with-fake-request ()
    (allow-cross-origin)
    (is (equal "*" (header :access-control-allow-origin)))))

(test nothing-extra-is-exposed-unless-asked-for
  "Expose-Headers widens what script can read, so it should not appear on
responses that have nothing to widen."
  (with-fake-request ()
    (allow-cross-origin)
    (is-false (header :access-control-expose-headers))))

(test a-named-header-can-be-exposed
  "The one that matters: without this the browser strips WWW-Authenticate
down to nothing and the client cannot find the authorization server."
  (with-fake-request ()
    (allow-cross-origin :expose "WWW-Authenticate")
    (is (equal "WWW-Authenticate" (header :access-control-expose-headers)))
    (is (equal "*" (header :access-control-allow-origin)))))

(test credentials-are-never-allowed
  "`*` and Allow-Credentials are mutually exclusive, and these endpoints
take no cookies -- a hostile page must not be able to borrow a session."
  (with-fake-request ()
    (allow-cross-origin :expose "WWW-Authenticate")
    (is-false (header :access-control-allow-credentials))))

;; ----------------------------------------------------------------------
;; Preflight
;; ----------------------------------------------------------------------

(test a-preflight-answers-204-with-no-body
  (with-fake-request ()
    (is (equal "" (preflight)))
    (is (equal 204 (hunchentoot:return-code*)))))

(test a-preflight-permits-the-headers-a-client-actually-sends
  (with-fake-request ()
    (preflight)
    (let ((allowed (header :access-control-allow-headers)))
      ;; The bearer token.
      (is-true (str:containsp "Authorization" allowed))
      ;; application/json is not CORS-safelisted, so JSON-RPC preflights
      ;; on content type alone.
      (is-true (str:containsp "Content-Type" allowed))
      ;; The MCP spec has clients send this on every call.
      (is-true (str:containsp "MCP-Protocol-Version" allowed)))))

(test a-preflight-names-the-methods-and-is-cacheable
  (with-fake-request ()
    (preflight)
    (is (equal "POST, OPTIONS" (header :access-control-allow-methods)))
    (is (equal "*" (header :access-control-allow-origin)))
    ;; Re-preflighting every call would double the request count for
    ;; values that never change.
    (is (< 0 (parse-integer (header :access-control-max-age))))))

(test a-preflight-can-name-other-methods
  (with-fake-request ()
    (preflight :methods "GET, OPTIONS")
    (is (equal "GET, OPTIONS" (header :access-control-allow-methods)))))

(test the-allowed-request-headers-are-a-comma-separated-list
  "Malformed here means the browser rejects every preflighted request,
which is a hard failure with a very indirect symptom."
  (let ((names (mapcar #'str:trim (str:split "," +allowed-request-headers+))))
    (is (< 1 (length names)))
    (is-true (every (lambda (name)
                      (and (not (str:emptyp name))
                           (not (str:containsp " " name))))
                    names))))

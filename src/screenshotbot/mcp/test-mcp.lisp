;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/test-mcp
  (:use #:cl
        #:fiveam)
  (:import-from #:core/installation/installation
                #:abstract-installation
                #:*installation*)
  (:import-from #:screenshotbot/mcp/mcp
                #:mcp-handler)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:documentation "End-to-end checks on the /mcp endpoint's rejection path.

The helpers are unit tested next door in the auth-server; what is worth
proving here is that they are actually wired into the handler, because
every one of these headers is invisible until a browser silently fails."))
(in-package :screenshotbot/mcp/test-mcp)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((*installation* (make-instance 'abstract-installation
                                         :domain "https://staging.screenshotbot.io")))
      (&body))))

(defun unauthenticated-post ()
  "POST /mcp with no credentials, returning the body."
  (with-fake-request (:script-name "/mcp")
    (setf (hunchentoot:return-code*) 200)
    (let ((body (mcp-handler)))
      (values body
              (hunchentoot:return-code*)
              (hunchentoot:headers-out*)))))

(defun header-of (headers name)
  (cdr (assoc name headers :test #'eq)))

(test an-unauthenticated-call-is-refused
  "/mcp was open to anyone who could reach the host."
  (with-fixture state ()
    (multiple-value-bind (body status) (unauthenticated-post)
      (declare (ignore body))
      (is (equal 401 status)))))

(test the-rejection-tells-a-client-where-to-authenticate
  (with-fixture state ()
    (multiple-value-bind (body status headers) (unauthenticated-post)
      (declare (ignore body status))
      (let ((challenge (header-of headers :www-authenticate)))
        (is-true challenge)
        (is-true (str:starts-with-p "Bearer " challenge))
        (is-true (str:containsp
                  "https://staging.screenshotbot.io/.well-known/oauth-protected-resource/mcp"
                  challenge))))))

(test the-rejection-is-readable-from-another-origin
  "A browser client lives on someone else's origin. Without these two
headers it gets an opaque failure: it cannot read the challenge, so it
cannot discover the authorization server, and nothing says why."
  (with-fixture state ()
    (multiple-value-bind (body status headers) (unauthenticated-post)
      (declare (ignore body status))
      (is (equal "*" (header-of headers :access-control-allow-origin)))
      (is (equal "WWW-Authenticate"
                 (header-of headers :access-control-expose-headers))))))

(test the-rejection-body-is-json-rpc
  (with-fixture state ()
    (let ((body (unauthenticated-post)))
      (is-true (str:containsp "\"jsonrpc\"" body))
      (is-true (str:containsp "\"id\":null" body)))))

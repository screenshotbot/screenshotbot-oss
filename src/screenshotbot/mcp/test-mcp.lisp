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
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:screenshotbot/mcp/mcp
                #:%dispatch
                #:+supported-protocol-versions+
                #:mcp-handler
                #:negotiate-protocol-version)
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

;; ----------------------------------------------------------------------
;; The JSON-RPC layer
;; ----------------------------------------------------------------------

(defun rpc-call (json)
  "Run one JSON-RPC request through the dispatcher, decoding the reply
with the member names left exactly as they go on the wire."
  (with-fake-request (:script-name "/mcp")
    (cl-mock:with-mocks ()
      (cl-mock:if-called 'hunchentoot:raw-post-data
                         (lambda (&rest args) (declare (ignore args)) json))
      (let ((body (%dispatch)))
        (values (unless (str:emptyp body)
                  (let ((json:*json-identifier-name-to-lisp* #'identity)
                        (json:*identifier-name-to-key* #'identity))
                    (json:decode-json-from-string body)))
                body)))))

(defun field (object name)
  (assoc-value object name :test #'equal))

(defun initialize (&optional (version "2025-11-25"))
  (field (rpc-call
          (format nil "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"initialize\",~
\"params\":{\"protocolVersion\":~s}}" version))
         "result"))

(test the-handshake-uses-the-member-names-the-spec-defines
  "CL-JSON downcases keyword keys, so these shipped as `protocolversion'
and `serverinfo'. A client looking for the camelCase names sees a
response with nothing in it and no error to report."
  (let ((result (initialize)))
    (is-true (field result "protocolVersion"))
    (is-true (field result "capabilities"))
    (is-true (field result "serverInfo"))
    (is (equal "Screenshotbot MCP Server"
               (field (field result "serverInfo") "name")))))

(test capabilities-is-an-object-not-an-array
  "It used to encode as [[\"tools\",[\"listchanged\"]]...] -- an array of
arrays, which is not a shape any client can read."
  (let ((capabilities (field (initialize) "capabilities")))
    ;; A decoded JSON object is an alist; an array would be a flat list.
    (is-true (every #'consp capabilities))
    (is-true (assoc "tools" capabilities :test #'equal))
    (is-true (assoc "resources" capabilities :test #'equal))))

(test only-capabilities-we-answer-are-advertised
  "prompts/list and logging are not implemented. Advertising them makes a
client fail on use rather than at discovery, which is harder to diagnose."
  (let ((capabilities (field (initialize) "capabilities")))
    (is-false (assoc "prompts" capabilities :test #'equal))
    (is-false (assoc "logging" capabilities :test #'equal))))

(test the-clients-protocol-version-is-echoed-when-we-support-it
  (dolist (version +supported-protocol-versions+)
    (is (equal version (field (initialize version) "protocolVersion")))))

(test an-unknown-protocol-version-gets-our-newest
  "Answering with a fixed old version regardless -- which is what it did --
reads to a modern client as a server it cannot talk to."
  (is (equal (first +supported-protocol-versions+)
             (negotiate-protocol-version "2026-07-28")))
  (is (equal (first +supported-protocol-versions+)
             (negotiate-protocol-version nil)))
  (is (equal (first +supported-protocol-versions+)
             (field (initialize "2026-07-28") "protocolVersion"))))

(test a-notification-gets-no-reply-at-all
  "JSON-RPC 2.0 §4.1: a request without an id must not be answered, not
even to say we did not understand it. notifications/initialized arrives
straight after the handshake, so replying makes the first thing a client
does look like a protocol violation."
  (multiple-value-bind (decoded body)
      (rpc-call "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
    (is-false decoded)
    (is (equal "" body))))

(test tools-list-returns-an-object-keyed-by-tools
  (let ((result (field (rpc-call
                        "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/list\"}")
                       "result")))
    (let ((tools (field result "tools")))
      (is (equal 1 (length tools)))
      (is (equal "list_channels" (field (first tools) "name")))
      ;; camelCase, and a schema whose `required' is an array rather than
      ;; the null CL-JSON produces for NIL.
      (let ((schema (field (first tools) "inputSchema")))
        (is-true schema)
        (is (equal "object" (field schema "type")))))))

(test resources-list-returns-an-object-keyed-by-resources
  (let ((result (field (rpc-call
                        "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"resources/list\"}")
                       "result")))
    (is (equal 1 (length (field result "resources"))))
    (is (equal "channel://list" (field (first (field result "resources")) "uri")))))

(test an-unknown-method-is-a-json-rpc-error-naming-the-method
  (let* ((response (rpc-call
                    "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"server/discover\"}"))
         (failure (field response "error")))
    (is (equal 1 (field response "id")))
    (is (equal -32601 (field failure "code")))
    ;; Naming it turns an unimplemented method into something greppable.
    (is-true (str:containsp "server/discover" (field failure "message")))))

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
                #:*tools*
                #:call-tool
                #:def-tool
                #:register-tool
                #:tool
                #:tool-result
                #:tool-definitions
                #:tool-description
                #:tool-name
                #:%dispatch
                #:+supported-protocol-versions+
                #:mcp-handler
                #:negotiate-protocol-version)
  (:import-from #:screenshotbot/mcp/test-util
                #:add-channel
                #:call-tool-as
                #:caller
                #:decode
                #:field
                #:post-as
                #:token
                #:token-with
                #:tool-text)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:documentation "The /mcp endpoint itself: who may reach it, and what
shape its JSON-RPC answers take.

What each individual tool then answers is tested next to that tool."))
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
                  (decode body))
                body)))))

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
  (let* ((result (field (rpc-call
                         "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/list\"}")
                        "result"))
         (tools (field result "tools"))
         ;; By name, not by position or count: adding a tool should not
         ;; break a test about the shape of the response.
         (channels (find "list_channels" tools
                         :key (lambda (tool) (field tool "name"))
                         :test #'equal)))
    (is-true tools)
    (is-true channels)
    ;; camelCase, and a schema whose `required' is an array rather than
    ;; the null CL-JSON produces for NIL.
    (let ((schema (field channels "inputSchema")))
      (is-true schema)
      (is (equal "object" (field schema "type"))))))

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

;; ----------------------------------------------------------------------
;; Scope, at the endpoint rather than in the helper
;; ----------------------------------------------------------------------

(test a-caller-holding-the-required-scope-reaches-the-dispatcher
  (with-fixture caller ()
    (multiple-value-bind (body status) (post-as (token-with '("api:read")))
      (is (equal 200 status))
      ;; It got as far as JSON-RPC, which is the point.
      (is-true (str:containsp "jsonrpc" body)))))

(test a-caller-without-the-required-scope-is-forbidden
  "/mcp advertises scopes_supported=[api:read] and, until this, never
checked it."
  (with-fixture caller ()
    (multiple-value-bind (body status headers) (post-as (token-with '("profile")))
      (declare (ignore body))
      (is (equal 403 status))
      (let ((challenge (header-of headers :www-authenticate)))
        (is-true (str:containsp "insufficient_scope" challenge))
        ;; Naming it is what lets the client ask for the right thing next.
        (is-true (str:containsp "scope=\"api:read\"" challenge))))))

(test calling-a-tool-without-the-scope-never-reaches-it
  "The scope check is at the endpoint, so it must stop a tools/call before
the tool runs -- not merely leave the result unread."
  (with-fixture caller ()
    (add-channel "secret-project")
    (multiple-value-bind (body status)
        (call-tool-as (token-with '("profile")) "list_channels")
      (is (equal 403 status))
      (is-false (str:containsp "secret-project" body)))))

;; ----------------------------------------------------------------------
;; Dispatching to a tool
;; ----------------------------------------------------------------------

(test calling-an-unknown-tool-is-a-protocol-error-not-a-tool-failure
  "-32602 rather than an isError result: the caller got the protocol
wrong, and there is no tool whose failure this could be."
  (with-fixture caller ()
    (let* ((response (decode (call-tool-as token "no_such_tool")))
           (failure (field response "error")))
      (is (equal -32602 (field failure "code")))
      (is-true (str:containsp "no_such_tool" (field failure "message")))
      (is-false (field response "result")))))

;; ----------------------------------------------------------------------
;; The tool registry
;; ----------------------------------------------------------------------

(test every-advertised-tool-is-callable
  "Structural since DEF-TOOL: advertising and dispatching read the same
registry, so this can no longer drift. Kept as a guard in case anyone
re-splits them, because a tool advertised but not dispatched is a
capability a model only discovers is missing when it tries to use it."
  (with-fixture caller ()
    (dolist (definition (tool-definitions))
      (let ((name (gethash "name" definition)))
        (is-true (nth-value 1 (call-tool name (list (cons "report_id" "x"))))
                 "advertised tool ~a does not dispatch" name)))))

(test every-advertised-tool-declares-an-object-schema
  (dolist (definition (tool-definitions))
    (let ((schema (gethash "inputSchema" definition)))
      (is (equal "object" (gethash "type" schema))
          "~a has no object inputSchema" (gethash "name" definition))
      ;; #() rather than NIL, which CL-JSON would render as null.
      (is-true (vectorp (gethash "required" schema))
               "~a declares required as a list, which encodes as null"
               (gethash "name" definition)))))

;; ----------------------------------------------------------------------
;; DEF-TOOL
;; ----------------------------------------------------------------------

(defun probe-tool (name description)
  (make-instance 'tool
                 :name name
                 :description description
                 :parameters nil
                 :handler (lambda (arguments)
                            (declare (ignore arguments))
                            description)))

(test redefining-a-tool-replaces-it-rather-than-adding-a-second
  "These files are reloaded into a running image. A registry that appended
would advertise every tool twice after the second load, and a client
would see duplicates with no way to tell which one it was calling."
  (let ((*tools* nil))
    (register-tool (probe-tool "probe" "first"))
    (register-tool (probe-tool "probe" "second"))
    (is (equal 1 (length *tools*)))
    (is (equal "second" (tool-description (first *tools*))))))

(test redefining-a-tool-keeps-its-position
  "Otherwise reloading would silently reorder what tools/list advertises."
  (let ((*tools* nil))
    (register-tool (probe-tool "a" "a"))
    (register-tool (probe-tool "b" "b"))
    (register-tool (probe-tool "a" "a-again"))
    (is (equal '("a" "b") (mapcar #'tool-name *tools*)))
    (is (equal "a-again" (tool-description (first *tools*))))))

;; fetch_report stands in for any DEF-TOOL-generated handler here: what
;; is under test is the macro's argument handling, not the report tool.

(test a-missing-required-argument-is-refused-by-name
  "DEF-TOOL generates the check, so the body never sees a blank argument.
Naming it is what lets a model fix its own call."
  (with-fixture caller ()
    (multiple-value-bind (text result)
        (tool-text (call-tool-as token "fetch_report" '(("report_id" . ""))))
      (is-true (field result "isError"))
      (is-true (str:containsp "report_id is required" text)))))

(test a-declared-parameter-reaches-the-advertised-schema
  "The schema is derived from the same declaration the handler binds, so
they cannot describe different arguments."
  (let* ((report-tool (find "fetch_report" (tool-definitions)
                            :key (lambda (tool) (gethash "name" tool))
                            :test #'equal))
         (schema (gethash "inputSchema" report-tool)))
    (is-true (gethash "report_id" (gethash "properties" schema)))
    (is (equal "report_id" (aref (gethash "required" schema) 0)))))

;; ----------------------------------------------------------------------
;; Per-tool scope
;; ----------------------------------------------------------------------

;; These register into a rebound *TOOLS* so the probe is invisible to
;; everything else -- notably EVERY-ADVERTISED-TOOL-IS-CALLABLE, which
;; walks the whole registry.

(test a-tool-declaring-a-scope-is-refused-when-the-token-lacks-it
  "The endpoint only requires api:read, whose consent line reads `Read
your runs, channels and reports'. Anything that writes has to ask for
more than the user agreed to on that screen."
  (with-fixture caller ()
    (let ((*tools* nil))
      (def-tool "probe_write" () :scope "api:write"
        "Probe."
        (tool-result "wrote"))
      (multiple-value-bind (text result)
          (tool-text (call-tool-as (token-with '("api:read")) "probe_write"))
        (is-true (field result "isError"))
        (is-false (str:containsp "wrote" text))))))

(test the-refusal-names-the-scope-that-would-let-it-through
  "Naming it is the only thing that tells the user what to reconnect with."
  (with-fixture caller ()
    (let ((*tools* nil))
      (def-tool "probe_write" () :scope "api:write"
        "Probe."
        (tool-result "wrote"))
      (let ((text (tool-text (call-tool-as (token-with '("api:read"))
                                           "probe_write"))))
        (is-true (str:containsp "api:write" text))))))

(test a-tool-declaring-a-scope-runs-when-the-token-carries-it
  (with-fixture caller ()
    (let ((*tools* nil))
      (def-tool "probe_write" () :scope "api:write"
        "Probe."
        (tool-result "wrote"))
      (multiple-value-bind (text result)
          (tool-text (call-tool-as (token-with '("api:read" "api:write"))
                                   "probe_write"))
        (is-false (field result "isError"))
        (is (equal "wrote" text))))))

(test a-tool-without-a-scope-still-runs-on-the-endpoint-scope-alone
  "The read tools must not have been made harder to reach by this."
  (with-fixture caller ()
    (multiple-value-bind (text result)
        (tool-text (call-tool-as (token-with '("api:read")) "list_channels"))
      (is-false (field result "isError"))
      (is (equal "[]" (str:trim text))))))

;; ----------------------------------------------------------------------
;; Parameters that may be empty
;; ----------------------------------------------------------------------

(test an-allow-empty-parameter-accepts-an-empty-value
  "`Set this to nothing' is a real request, and the required-argument check
would otherwise make it unexpressible."
  (with-fixture caller ()
    (let ((*tools* nil))
      (def-tool "probe_empty" ((value "value" "Anything" :allow-empty t))
        "Probe."
        (tool-result (format nil "got ~s" value)))
      (multiple-value-bind (text result)
          (tool-text (call-tool-as token "probe_empty" '(("value" . ""))))
        (is-false (field result "isError"))
        (is (equal "got \"\"" text))))))

(test an-absent-allow-empty-parameter-arrives-as-the-empty-string
  "So a tool body has one case to handle rather than two."
  (with-fixture caller ()
    (let ((*tools* nil))
      (def-tool "probe_empty" ((value "value" "Anything" :allow-empty t))
        "Probe."
        (tool-result (format nil "got ~s" value)))
      (let ((text (tool-text (call-tool-as token "probe_empty"))))
        (is (equal "got \"\"" text))))))

(test an-allow-empty-parameter-is-still-advertised-as-required
  "It must be *present*; it may be empty. Dropping it from `required'
would tell a client the argument is optional, which is a different claim."
  (let ((*tools* nil))
    (def-tool "probe_empty" ((value "value" "Anything" :allow-empty t))
      "Probe."
      (tool-result value))
    (let ((schema (gethash "inputSchema" (first (tool-definitions)))))
      (is (equal "value" (aref (gethash "required" schema) 0)))
      (is-true (gethash "value" (gethash "properties" schema))))))

(test a-parameter-not-marked-allow-empty-still-refuses-an-empty-value
  (with-fixture caller ()
    (let ((*tools* nil))
      (def-tool "probe_strict" ((value "value" "Anything"))
        "Probe."
        (tool-result value))
      (multiple-value-bind (text result)
          (tool-text (call-tool-as token "probe_strict" '(("value" . ""))))
        (is-true (field result "isError"))
        (is-true (str:containsp "value is required" text))))))

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
  (:import-from #:screenshotbot/api/core
                #:authenticate-api-request
                #:authenticate-request-from-key
                #:bearer-token)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:screenshotbot/model/company
                #:company-channels)
  (:import-from #:screenshotbot/auth-server/model
                #:make-access-token
                #:oauth-grant
                #:register-oauth-client)
  (:import-from #:screenshotbot/auth-server/protected-resource
                #:mcp-resource-identifier)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
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

;; ----------------------------------------------------------------------
;; Scope, at the endpoint rather than in the helper
;; ----------------------------------------------------------------------

(defparameter +initialize+
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}")

(defun post-as (api-key &key (json +initialize+))
  "POST /mcp with API-KEY as the authenticated caller.

Headers are read inside the fake request: HUNCHENTOOT:*REPLY* is unbound
outside it, and the failure looks nothing like the cause."
  (with-fake-request (:script-name "/mcp")
    (cl-mock:with-mocks ()
      (cl-mock:if-called 'bearer-token (lambda () "a-token"))
      ;; Stub only the credential *extraction*. The real function also
      ;; binds the user, the account and the viewer context on the
      ;; request, and every one of those is something the tools read --
      ;; a mock that skipped them would test a caller who is
      ;; authenticated as nobody.
      (cl-mock:if-called 'authenticate-api-request
                         (lambda (request)
                           (authenticate-request-from-key request api-key)))
      (cl-mock:if-called 'hunchentoot:raw-post-data
                         (lambda (&rest args) (declare (ignore args)) json))
      (setf (hunchentoot:return-code*) 200)
      (let ((body (mcp-handler)))
        (values body
                (hunchentoot:return-code*)
                (hunchentoot:headers-out*))))))

(def-fixture caller ()
  ;; One fixture rather than two nested ones: FiveAM's &BODY is a macrolet,
  ;; so a DEF-FIXTURE whose body uses WITH-FIXTURE shadows its own marker
  ;; and the inner (&BODY) resolves to the wrong one.
  (with-test-store ()
    (with-test-user (:company company :user user)
      (let ((*installation* (make-instance 'abstract-installation
                                           :domain "https://staging.screenshotbot.io"))
            (client (register-oauth-client :client-id "c"
                                           :scopes (list "profile" "api:read"))))
        (flet ((token-with (scopes)
                 (make-access-token
                  (make-instance 'oauth-grant :client client :user user
                                              :company company :scopes scopes)
                  :scopes scopes
                  :resource (mcp-resource-identifier)))
               (add-channel (name)
                 (let ((channel (make-instance 'channel :name name
                                                        :company company)))
                   (push channel (company-channels company))
                   channel)))
          (let ((token (token-with '("api:read"))))
            (declare (ignorable token))
            (&body)))))))

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

;; ----------------------------------------------------------------------
;; tools/call
;; ----------------------------------------------------------------------

(defun call-tool-as (api-key name)
  (post-as api-key
           :json (format nil "{\"jsonrpc\":\"2.0\",\"id\":9,~
\"method\":\"tools/call\",\"params\":{\"name\":~s,\"arguments\":{}}}"
                         name)))

(defun tool-text (body)
  "The text content out of a tools/call result."
  (let* ((response (let ((json:*json-identifier-name-to-lisp* #'identity)
                         (json:*identifier-name-to-key* #'identity))
                     (json:decode-json-from-string body)))
         (result (field response "result")))
    (values (field (first (field result "content")) "text")
            result
            response)))

(test listing-channels-returns-them-as-json-with-names-and-urls
  (with-fixture caller ()
    (add-channel "beta")
    (add-channel "alpha")
    (multiple-value-bind (text result)
        (tool-text (call-tool-as token "list_channels"))
      ;; No isError on a successful call -- the spec defaults it, and
      ;; saying "false" in CL-JSON would mean saying null.
      (is-false (field result "isError"))
      (let ((channels (let ((json:*json-identifier-name-to-lisp* #'identity)
                            (json:*identifier-name-to-key* #'identity))
                        (json:decode-json-from-string text))))
        (is (equal 2 (length channels)))
        ;; Sorted, so the output does not reshuffle between calls for no
        ;; reason a reader could see.
        (is (equal "alpha" (field (first channels) "name")))
        (is (equal "beta" (field (second channels) "name")))
        (is-true (str:containsp "/channels/"
                                (field (first channels) "url")))))))

(test an-account-with-no-channels-gets-an-empty-list-not-an-error
  "A model handles [] fine; it handles a tool failure by giving up."
  (with-fixture caller ()
    (multiple-value-bind (text result) (tool-text (call-tool-as token "list_channels"))
      (is-false (field result "isError"))
      (is (equal "[]" (str:trim text))))))

(test channels-belonging-to-another-account-are-not-listed
  (with-fixture caller ()
    (add-channel "ours")
    (let ((other (make-instance 'screenshotbot/model/company:company
                                :name "someone else")))
      (make-instance 'channel :name "theirs" :company other))
    (let ((text (tool-text (call-tool-as token "list_channels"))))
      (is-true (str:containsp "ours" text))
      (is-false (str:containsp "theirs" text)))))

(test a-truncated-listing-says-so
  "A model that cannot see the cut will report a partial list as the
whole one."
  (with-fixture caller ()
    (dotimes (i 3)
      (add-channel (format nil "channel-~a" i)))
    (let ((max-channels 2))
      (let ((text (progv (list 'screenshotbot/mcp/mcp::+max-channels+)
                      (list max-channels)
                    (tool-text (call-tool-as token "list_channels")))))
        (is-true (str:containsp "Showing the first 2 of 3 channels" text))
        (is-true (str:containsp "channel-0" text))
        (is-false (str:containsp "channel-2" text))))))

(test an-untruncated-listing-says-nothing-about-truncation
  (with-fixture caller ()
    (add-channel "only-one")
    (let ((text (tool-text (call-tool-as token "list_channels"))))
      (is-false (str:containsp "Showing the first" text)))))

(test calling-an-unknown-tool-is-a-protocol-error-not-a-tool-failure
  "-32602 rather than an isError result: the caller got the protocol
wrong, and there is no tool whose failure this could be."
  (with-fixture caller ()
    (let* ((body (call-tool-as token "no_such_tool"))
           (response (let ((json:*json-identifier-name-to-lisp* #'identity)
                           (json:*identifier-name-to-key* #'identity))
                       (json:decode-json-from-string body)))
           (failure (field response "error")))
      (is (equal -32602 (field failure "code")))
      (is-true (str:containsp "no_such_tool" (field failure "message")))
      (is-false (field response "result")))))

(test calling-a-tool-without-the-scope-never-reaches-it
  (with-fixture caller ()
    (add-channel "secret-project")
    (multiple-value-bind (body status)
        (call-tool-as (token-with '("profile")) "list_channels")
      (is (equal 403 status))
      (is-false (str:containsp "secret-project" body)))))

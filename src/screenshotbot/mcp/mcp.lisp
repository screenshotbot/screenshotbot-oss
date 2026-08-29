;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/mcp
  (:use #:cl)
  (:import-from #:screenshotbot/auth-server/cors
                #:allow-cross-origin
                #:preflight)
  (:import-from #:screenshotbot/auth-server/protected-resource
                #:mcp-resource-identifier
                #:mcp-resource-metadata-url)
  (:import-from #:screenshotbot/auth-server/resource-server
                #:with-bearer-authentication)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:json
                #:encode-json-to-string)
  (:export
   #:mcp-handler))
(in-package :screenshotbot/mcp/mcp)

(defparameter +mcp-scope+ "api:read"
  "What a caller must have agreed to. Matches scopes_supported in the
protected resource document; the two have to say the same thing or a
client is told to ask for something that will not let it in.")

(defparameter +supported-protocol-versions+
  '("2025-11-25" "2025-06-18" "2025-03-26" "2024-11-05")
  "Newest first. Our JSON-RPC surface is the same under all of them.

MCP negotiation says: echo the client's version if we support it,
otherwise answer with one we do and let the client decide. Answering with
a fixed old version regardless -- which is what this did -- reads to a
modern client as a server it cannot talk to.")

(defun obj (&rest plist)
  "Build a JSON object with the keys exactly as written.

Hash tables rather than alists on purpose. CL-JSON downcases keyword keys
\(so :protocolVersion ships as \"protocolversion\") and encodes an alist as
an array of arrays rather than an object. Both are silent, and both make
the response unintelligible to a client that is being polite about it."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash key table) value))
    table))

(defun %result (id result)
  (encode-json-to-string (obj "jsonrpc" "2.0" "id" id "result" result)))

(defun %error (id code message)
  (encode-json-to-string
   (obj "jsonrpc" "2.0" "id" id
        "error" (obj "code" code "message" message))))

(defun negotiate-protocol-version (requested)
  (if (member requested +supported-protocol-versions+ :test #'equal)
      requested
      (first +supported-protocol-versions+)))

(defun initialize-result (requested-version)
  (obj "protocolVersion" (negotiate-protocol-version requested-version)
       ;; Only what we actually answer. Advertising prompts and logging
       ;; while `prompts/list' returns method-not-found is worse than
       ;; staying quiet: the client believes the capability is there and
       ;; fails on use rather than at discovery.
       "capabilities" (obj "tools" (obj)
                           "resources" (obj))
       "serverInfo" (obj "name" "Screenshotbot MCP Server"
                         "version" "1.0.0")))

(defun list-tools ()
  (obj "tools"
       (list (obj "name" "list_channels"
                  "description" "List all channels (projects) in Screenshotbot"
                  ;; #() not NIL: CL-JSON renders NIL as null, and a
                  ;; JSON Schema `required' of null is invalid where an
                  ;; empty array is fine.
                  "inputSchema" (obj "type" "object"
                                     "properties" (obj)
                                     "required" #())))))

(defun list-resources ()
  (obj "resources"
       (list (obj "uri" "channel://list"
                  "name" "channels"
                  "description" "List of all channels (projects) in Screenshotbot"
                  "mimeType" "application/json"))))

(defun %dispatch ()
  (setf (hunchentoot:header-out :content-type) "application/json")
  (let* ((request-body (hunchentoot:raw-post-data :force-text t))
         (request-json (when request-body
                         (cl-json:decode-json-from-string request-body)))
         (method (cdr (assoc :method request-json)))
         (id (cdr (assoc :id request-json)))
         (params (cdr (assoc :params request-json))))
    (log:info "Got body: ~a" request-body)
    (cond
      ;; JSON-RPC 2.0 §4.1: a request without an id is a notification, and
      ;; a server MUST NOT reply to one -- not even to say it did not
      ;; understand. `notifications/initialized' arrives right after the
      ;; handshake, so answering it makes the very first thing a client
      ;; does look like a protocol violation.
      ((null id)
       (setf (hunchentoot:return-code*) hunchentoot:+http-accepted+)
       "")
      ((equal method "initialize")
       (%result id (initialize-result
                    (cdr (assoc :protocol-version params)))))
      ((equal method "tools/list")
       (%result id (list-tools)))
      ((equal method "resources/list")
       (%result id (list-resources)))
      (t
       (%error id -32601 (format nil "Method not found: ~a" method))))))

(defhandler (mcp-handler :uri "/mcp" :method :post) ()
  ;; Before the auth wrapper, so these land on the 401 as well as the 200.
  ;; WWW-Authenticate is the header a browser client needs in order to
  ;; discover the authorization server, and it is not CORS-safelisted --
  ;; without the expose header the browser strips it and the client is
  ;; left with an opaque rejection.
  (allow-cross-origin :expose "WWW-Authenticate")
  ;; Every method is behind the token, including `initialize': the MCP
  ;; authorization spec protects the endpoint, not individual calls.
  (with-bearer-authentication (:resource-metadata-url (mcp-resource-metadata-url)
                               :resource (mcp-resource-identifier)
                               ;; The scope the protected-resource document
                               ;; has been advertising all along. Until now
                               ;; nothing checked it, which was survivable
                               ;; only because /mcp returned static stubs.
                               :scope +mcp-scope+)
    (%dispatch)))

(defhandler (nil :uri "/mcp" :method :options) ()
  (preflight))

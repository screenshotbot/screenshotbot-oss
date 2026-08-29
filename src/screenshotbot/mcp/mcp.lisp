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
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain)
  (:import-from #:screenshotbot/model/channel
                #:channel-name)
  (:import-from #:screenshotbot/model/company
                #:company-channels)
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

(defparameter +max-channels+ 200
  "Cap on how many channels one call returns. A company with thousands of
them would otherwise produce a result no model can use and no reviewer
would enjoy reading in a log.")

(defun list-tools ()
  (obj "tools"
       (list (obj "name" "list_channels"
                  "description"
                  "List the channels (projects) in the authenticated Screenshotbot account. Returns JSON: an array of objects with `name` and `url`."
                  ;; #() not NIL: CL-JSON renders NIL as null, and a
                  ;; JSON Schema `required' of null is invalid where an
                  ;; empty array is fine.
                  "inputSchema" (obj "type" "object"
                                     "properties" (obj)
                                     "required" #())))))

;; ----------------------------------------------------------------------
;; Tools
;; ----------------------------------------------------------------------

(defun tool-result (text &key errorp)
  "An MCP tool result.

A tool that fails reports it *in the result* with isError, not as a
JSON-RPC error. JSON-RPC errors mean the protocol broke; a tool failing
is an answer the model should see and react to, and burying it in a
transport error hides it from the model entirely."
  (let ((result (obj "content" (list (obj "type" "text" "text" text)))))
    (when errorp
      ;; Omitted rather than set to false in the ordinary case: the spec
      ;; defaults it, and CL-JSON has no way to say false without saying
      ;; null, which is not the same thing.
      (setf (gethash "isError" result) t))
    result))

(defun channel-url (channel)
  (format nil "~a/channels/~a"
          (string-right-trim "/" (installation-domain *installation*))
          (store-object-id channel)))

(defun visible-channels (company)
  "CHANNELS of COMPANY this caller may see, in a stable order.

The viewer-context check is belt and braces -- every channel here belongs
to the company the token authenticated as -- but listing objects without
asking is the habit that eventually lists the wrong ones."
  (let ((viewer (auth:viewer-context hunchentoot:*request*)))
    (sort
     (remove-if-not (lambda (channel)
                      (auth:can-viewer-view viewer channel))
                    (company-channels company))
     #'string<
     :key #'channel-name)))

(defun list-channels-tool ()
  (let ((company (auth:current-company)))
    (unless company
      (return-from list-channels-tool
        (tool-result "This token is not associated with an account."
                     :errorp t)))
    (let* ((channels (visible-channels company))
           (shown (if (> (length channels) +max-channels+)
                      (subseq channels 0 +max-channels+)
                      channels)))
      (tool-result
       (format nil "~a~@[~%~%~a~]"
               (encode-json-to-string
                ;; A vector, so that no channels renders as [] rather than
                ;; the null CL-JSON gives for an empty list. A model
                ;; reading null has been told something quite different
                ;; from "there are none".
                (coerce (mapcar (lambda (channel)
                                  (obj "name" (channel-name channel)
                                       "url" (channel-url channel)))
                                shown)
                        'vector))
               ;; Say so rather than silently truncating: a model that
               ;; cannot see the cut will confidently report a partial
               ;; list as the whole one.
               (when (> (length channels) +max-channels+)
                 (format nil "Showing the first ~a of ~a channels."
                         +max-channels+ (length channels))))))))

(defun call-tool (name)
  "Run the named tool. Second value is NIL if there is no such tool."
  (cond
    ((equal name "list_channels")
     (values (list-channels-tool) t))
    (t
     (values nil nil))))

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
      ((equal method "tools/call")
       (let ((name (cdr (assoc :name params))))
         (multiple-value-bind (result foundp) (call-tool name)
           (if foundp
               (%result id result)
               ;; No such tool is a caller mistake about the protocol, not
               ;; a tool failing, so it is a JSON-RPC error rather than an
               ;; isError result.
               (%error id -32602 (format nil "No such tool: ~a" name))))))
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

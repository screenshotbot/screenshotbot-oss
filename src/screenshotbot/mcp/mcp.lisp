;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/mcp
  (:use #:cl)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain)
  (:import-from #:screenshotbot/auth-server/cors
                #:allow-cross-origin
                #:preflight)
  (:import-from #:screenshotbot/auth-server/protected-resource
                #:mcp-resource-identifier
                #:mcp-resource-metadata-url)
  (:import-from #:screenshotbot/auth-server/resource-server
                #:token-has-scope-p
                #:with-bearer-authentication)
  (:import-from #:auth/viewer-context
                #:api-viewer-context
                #:viewer-context-api-key)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:json
                #:encode-json-to-string)
  (:export
   #:mcp-handler
   ;; What a tool file needs. DEF-TOOL's expansion also reaches FIELD,
   ;; TOOL-RESULT and STR:EMPTYP, but by symbol identity rather than
   ;; through the using package, so those need no import to work --
   ;; TOOL-RESULT is exported because tool bodies call it directly.
   #:capped
   #:dashboard-url
   #:def-tool
   #:obj
   #:tool-result
   #:visible-to-caller))
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

(defun dashboard-url (path id)
  "An absolute URL for an object's page on this installation.

Absolute because the reader is a model somewhere else on the internet: a
site-relative path is one it cannot follow and cannot repair."
  (format nil "~a/~a/~a"
          (string-right-trim "/" (installation-domain *installation*))
          path id))

(defun capped (items max renderer)
  "Render at most MAX of ITEMS with RENDERER, as a JSON array.

The second value is the true count, and only when it exceeded MAX, so a
caller can say so rather than let a short list imply completeness. A model
that cannot see the cut reports a partial list as the whole one, which is
worse than refusing to answer.

A vector, not a list: CL-JSON renders an empty list as null, and a model
told `null' has been told something quite different from `there are
none'."
  (let ((shown (if (> (length items) max)
                   (subseq items 0 max)
                   items)))
    (values (coerce (mapcar renderer shown) 'vector)
            (when (> (length items) max)
              (length items)))))

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

(defvar *tools* nil
  "Every MCP tool, in definition order. DEF-TOOL registers here.

One list rather than two: advertising and dispatching used to be separate
lists that could drift, and a tool advertised but not dispatched is a
capability a model only discovers is missing when it tries to use it.")

(defclass tool ()
  ((name :initarg :name
         :reader tool-name
         :documentation "The name on the wire.")
   (description :initarg :description
                :reader tool-description
                :documentation "What the model reads to decide whether to call this.")
   (parameters :initarg :parameters
               :reader tool-parameters
               :documentation "A list of (JSON-NAME DESCRIPTION). All required.")
   (scope :initarg :scope
          :initform nil
          :reader tool-scope
          :documentation "An OAuth scope the caller must hold on top of the
endpoint's own, or NIL. Every read tool is NIL: the endpoint already
requires +MCP-SCOPE+, and asking twice for the same thing buys nothing.")
   (handler :initarg :handler
            :reader tool-handler)))

(defun register-tool (tool)
  "Add TOOL, replacing any existing tool of the same name.

Replacing rather than pushing matters: this file gets reloaded into a
running image, and a registry that appended would advertise every tool
twice after the second load."
  (let ((existing (position (tool-name tool) *tools*
                            :key #'tool-name :test #'equal)))
    (if existing
        (setf (nth existing *tools*) tool)
        (setf *tools* (append *tools* (list tool))))
    tool))

(defmacro def-tool (name (&rest parameters) &body options-description-and-body)
  "Define an MCP tool called NAME.

Each parameter is (VARIABLE JSON-NAME DESCRIPTION &key ALLOW-EMPTY) and
is required: the generated handler answers with a tool error when one is
missing, so BODY never sees a blank argument. ALLOW-EMPTY lifts that for
a parameter whose empty value means something -- `set this to nothing' is
a real request, and without it there is no way to express it. Such a
parameter is still advertised as required, because it must be *present*;
it just may be empty. An absent one arrives as \"\" rather than NIL, so
BODY has one case to handle instead of two.

Keyword options may follow the parameter list, before the description:

  :scope   an OAuth scope required on top of the endpoint's own. Use it
           for anything that writes. Without it a tool inherits only
           +MCP-SCOPE+, which the consent screen describes as read
           access.

DESCRIPTION is a required positional rather than a docstring because it
is the only thing a model reads when deciding whether to call this at
all, and a docstring is the sort of thing that gets dropped."
  (let ((arguments (gensym "ARGUMENTS"))
        (options nil))
    (loop while (keywordp (first options-description-and-body))
          do (push (pop options-description-and-body) options)
             (push (pop options-description-and-body) options))
    (setf options (nreverse options))
    (let ((description (pop options-description-and-body))
          (body options-description-and-body))
      (flet ((allow-empty-p (parameter)
               (getf (cdddr parameter) :allow-empty)))
        `(register-tool
          (make-instance 'tool
                         :name ,name
                         :description ,description
                         :scope ,(getf options :scope)
                         :parameters ',(loop for (nil json-name parameter-description)
                                               in parameters
                                             collect (list json-name
                                                           parameter-description))
                         :handler
                         (lambda (,arguments)
                           (declare (ignorable ,arguments))
                           (let ,(loop for parameter in parameters
                                       for (variable json-name) = parameter
                                       collect
                                       (if (allow-empty-p parameter)
                                           `(,variable (or (field ,arguments ,json-name) ""))
                                           `(,variable (field ,arguments ,json-name))))
                             (cond
                               ,@(loop for parameter in parameters
                                       for (variable json-name) = parameter
                                       unless (allow-empty-p parameter)
                                         collect `((str:emptyp ,variable)
                                                   (tool-result
                                                    ,(format nil "~a is required." json-name)
                                                    :errorp t)))
                               (t ,@body))))))))))

(defun tool-schema (tool)
  (obj "type" "object"
       "properties"
       (apply #'obj
              (loop for (json-name description) in (tool-parameters tool)
                    append (list json-name
                                 (obj "type" "string"
                                      "description" description))))
       ;; #() rather than a list, which CL-JSON renders as null.
       "required" (coerce (mapcar #'first (tool-parameters tool)) 'vector)))

(defun tool-definitions ()
  (mapcar (lambda (tool)
            (obj "name" (tool-name tool)
                 "description" (tool-description tool)
                 "inputSchema" (tool-schema tool)))
          *tools*))

(defun list-tools ()
  (obj "tools" (tool-definitions)))

(defun caller-has-scope-p (scope)
  "Does the token behind this request carry SCOPE?

Read off the viewer context rather than re-parsing the header: the
endpoint has already authenticated, and asking the same question twice is
how the two answers start disagreeing.

Fails closed when there is no request at all, which is what a tool called
from a REPL or a test harness looks like. Refusing is the safe answer,
and it beats an UNBOUND-VARIABLE from deep inside a tool."
  (let ((viewer (and (boundp 'hunchentoot:*request*)
                     hunchentoot:*request*
                     (auth:viewer-context hunchentoot:*request*))))
    (and (typep viewer 'api-viewer-context)
         (token-has-scope-p (viewer-context-api-key viewer) scope))))

(defun call-tool (name arguments)
  "Run the named tool. Second value is NIL if there is no such tool."
  (let ((tool (find name *tools* :key #'tool-name :test #'equal)))
    (cond
      ((null tool)
       (values nil nil))
      ((and (tool-scope tool)
            (not (caller-has-scope-p (tool-scope tool))))
       ;; Checked here rather than in the generated handler so that every
       ;; tool in the registry is covered by construction, including ones
       ;; added later by someone who did not read this comment.
       (values
        (tool-result
         (format nil "The ~a tool needs the ~s scope, which this connection was not granted. Reconnect to Screenshotbot and approve it to use this tool."
                 name (tool-scope tool))
         :errorp t)
        t))
      (t
       (values (funcall (tool-handler tool) arguments) t)))))

(defun visible-to-caller (object type)
  "OBJECT, if it is of TYPE and this caller may see it. Otherwise NIL."
  (when (and (typep object type)
             (auth:can-viewer-view (auth:viewer-context hunchentoot:*request*)
                                   object))
    object))

(defun list-resources ()
  (obj "resources"
       (list (obj "uri" "channel://list"
                  "name" "channels"
                  "description" "List of all channels (projects) in Screenshotbot"
                  "mimeType" "application/json"))))

(defun decode-request (raw)
  "Decode a JSON-RPC request with member names left as strings.

Same reason as OBJ on the way out. CL-JSON's default mapping turns
protocolVersion into :PROTOCOL-VERSION and would turn report_id into
something you have to go and check -- and every such guess is a bug that
only shows up against a real client."
  (let ((json:*json-identifier-name-to-lisp* #'identity)
        (json:*identifier-name-to-key* #'identity))
    (json:decode-json-from-string raw)))

(defun field (object name)
  (cdr (assoc name object :test #'equal)))

(defun %dispatch ()
  (setf (hunchentoot:header-out :content-type) "application/json")
  (let* ((request-body (hunchentoot:raw-post-data :force-text t))
         (request-json (unless (str:emptyp request-body)
                         (decode-request request-body)))
         (method (field request-json "method"))
         (id (field request-json "id"))
         (params (field request-json "params")))
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
       (%result id (initialize-result (field params "protocolVersion"))))
      ((equal method "tools/list")
       (%result id (list-tools)))
      ((equal method "resources/list")
       (%result id (list-resources)))
      ((equal method "tools/call")
       (let ((name (field params "name")))
         (multiple-value-bind (result foundp)
             (call-tool name (field params "arguments"))
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

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
  (:import-from #:screenshotbot/diff-report
                #:after
                #:before
                #:diff-report-added
                #:diff-report-changes
                #:diff-report-deleted
                #:make-diff-report)
  (:import-from #:screenshotbot/model/channel
                #:channel-name)
  (:import-from #:screenshotbot/model/image
                #:find-image-by-oid
                #:image
                #:image-public-url)
  (:import-from #:screenshotbot/model/report
                #:report
                #:report-channel
                #:report-previous-run
                #:report-run
                #:report-title)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot-image
                #:screenshot-name)
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

(defmacro def-tool (name (&rest parameters) description &body body)
  "Define an MCP tool called NAME.

Each parameter is (VARIABLE JSON-NAME DESCRIPTION) and is required: the
generated handler answers with a tool error when one is missing, so BODY
never sees a blank argument.

DESCRIPTION is a required positional rather than a docstring because it
is the only thing a model reads when deciding whether to call this at
all, and a docstring is the sort of thing that gets dropped."
  (let ((arguments (gensym "ARGUMENTS")))
    `(register-tool
      (make-instance 'tool
                     :name ,name
                     :description ,description
                     :parameters ',(loop for (nil json-name parameter-description)
                                           in parameters
                                         collect (list json-name
                                                       parameter-description))
                     :handler
                     (lambda (,arguments)
                       (declare (ignorable ,arguments))
                       (let ,(loop for (variable json-name) in parameters
                                   collect `(,variable (field ,arguments ,json-name)))
                         (cond
                           ,@(loop for (variable json-name) in parameters
                                   collect `((str:emptyp ,variable)
                                             (tool-result
                                              ,(format nil "~a is required." json-name)
                                              :errorp t)))
                           (t ,@body))))))))

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

(defun call-tool (name arguments)
  "Run the named tool. Second value is NIL if there is no such tool."
  (let ((tool (find name *tools* :key #'tool-name :test #'equal)))
    (if tool
        (values (funcall (tool-handler tool) arguments) t)
        (values nil nil))))

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

(defun list-channels-result (company)
  (let* ((channels (visible-channels company))
         (shown (if (> (length channels) +max-channels+)
                    (subseq channels 0 +max-channels+)
                    channels)))
    (tool-result
     (format nil "~a~@[~%~%~a~]"
             (encode-json-to-string
              ;; A vector, so that no channels renders as [] rather than
              ;; the null CL-JSON gives for an empty list. A model reading
              ;; null has been told something quite different from "there
              ;; are none".
              (coerce (mapcar (lambda (channel)
                                (obj "name" (channel-name channel)
                                     "url" (channel-url channel)))
                              shown)
                      'vector))
             ;; Say so rather than silently truncating: a model that
             ;; cannot see the cut will confidently report a partial list
             ;; as the whole one.
             (when (> (length channels) +max-channels+)
               (format nil "Showing the first ~a of ~a channels."
                       +max-channels+ (length channels)))))))

(def-tool "list_channels" ()
    "List the channels (projects) in the authenticated Screenshotbot account. Returns JSON: an array of objects with `name` and `url`."
  (let ((company (auth:current-company)))
    (cond
      ((null company)
       (tool-result "This token is not associated with an account."
                    :errorp t))
      (t
       (list-channels-result company)))))

(defparameter +max-changes+ 100
  "Cap on screenshots reported per section. Same reasoning as
+MAX-CHANNELS+: a 2000-screenshot report helps nobody.")

(defun visible (object type)
  "OBJECT, if it is of TYPE and this caller may see it. Otherwise NIL."
  (when (and (typep object type)
             (auth:can-viewer-view (auth:viewer-context hunchentoot:*request*)
                                   object))
    object))

(defun find-report-by-id (id)
  "The report with ID, if this caller may see it.

Never signals. A model hands us whatever string it has, and a malformed
id has to come back as something it can read and correct rather than as
an internal error it can only retry."
  (visible (ignore-errors (util:find-by-oid id 'report)) 'report))

(defun find-image-by-id (id)
  "The image with ID, if this caller may see it.

Images are not in the generic object-id index -- they carry their own oid
and their own lookup -- so this cannot go through FIND-BY-OID, which
simply returns NIL for every image id."
  (visible (ignore-errors (find-image-by-oid id)) 'image))

(defun screenshot-json (screenshot)
  (let ((image (screenshot-image screenshot)))
    (obj "name" (screenshot-name screenshot)
         ;; The id rather than the URL: a report can carry hundreds of
         ;; screenshots, and a model should spend a call only on the ones
         ;; it decides to look at. fetch_image_url resolves them.
         "imageId" (when image (util:oid image)))))

(defun change-json (change)
  (obj "name" (screenshot-name (after change))
       "before" (screenshot-json (before change))
       "after" (screenshot-json (after change))))

(defun capped (items renderer)
  "Render at most +MAX-CHANGES+ of ITEMS, and say so when there are more."
  (let ((shown (if (> (length items) +max-changes+)
                   (subseq items 0 +max-changes+)
                   items)))
    (values (coerce (mapcar renderer shown) 'vector)
            (when (> (length items) +max-changes+)
              (length items)))))

(defun report-json (report)
  (let* ((run (report-run report))
         (previous (report-previous-run report))
         (diff-report (when (and run previous)
                        (make-diff-report run previous))))
    (multiple-value-bind (changed changed-total)
        (capped (if diff-report (diff-report-changes diff-report) nil)
                #'change-json)
      (multiple-value-bind (added added-total)
          (capped (if diff-report (diff-report-added diff-report) nil)
                  #'screenshot-json)
        (multiple-value-bind (deleted deleted-total)
            (capped (if diff-report (diff-report-deleted diff-report) nil)
                    #'screenshot-json)
          (let ((result
                  (obj "id" (util:oid report)
                       "title" (report-title report)
                       "channel" (let ((channel (report-channel report)))
                                   (when channel (channel-name channel)))
                       "url" (format nil "~a/report/~a"
                                     (string-right-trim
                                      "/" (installation-domain *installation*))
                                     (util:oid report))
                       "run" (when run (util:oid run))
                       "previousRun" (when previous (util:oid previous))
                       "changed" changed
                       "added" added
                       "deleted" deleted)))
            ;; Truncation is stated, not implied by a short list: a model
            ;; that cannot see the cut reports a partial diff as the whole
            ;; one, which is worse than refusing to answer.
            (loop for (key total) in (list (list "changedTotal" changed-total)
                                           (list "addedTotal" added-total)
                                           (list "deletedTotal" deleted-total))
                  if total
                    do (setf (gethash key result) total))
            result))))))

(def-tool "fetch_report"
    ((id "report_id" "The report id, as it appears in a report URL"))
    "Fetch a Screenshotbot report by id, describing what changed between two runs. Returns JSON with the report metadata and, for each screenshot, the ids of the before and after images. Use fetch_image_url to turn an image id into a URL you can look at."
  (let ((report (find-report-by-id id)))
    (cond
      ((null report)
       ;; Deliberately one message for "no such report" and "not yours".
       ;; Distinguishing them would let a caller enumerate which report
       ;; ids exist.
       (tool-result
        (format nil "No report ~a is visible to this account." id)
        :errorp t))
      (t
       (tool-result (encode-json-to-string (report-json report)))))))

(defun image-url (image)
  "A publicly fetchable URL for IMAGE.

IMAGE-PUBLIC-URL can return a site-relative path, which is useless to a
model on the other side of the internet. Binding *CDN-DOMAIN* the way the
run API does makes MAKE-CDN absolutize it."
  (let ((util.cdn:*cdn-domain* (or util.cdn:*cdn-domain*
                                   (installation-domain *installation*))))
    (util.cdn:make-cdn (image-public-url image :originalp t))))

(def-tool "fetch_image_url"
    ((id "image_id" "An image id, as returned by fetch_report"))
    "Resolve a Screenshotbot image id into a URL. Image ids come from fetch_report. Returns JSON with a `url` you can fetch or view."
  (let ((image (find-image-by-id id)))
    (cond
      ((null image)
       ;; One answer for missing and forbidden, as with reports.
       (tool-result
        (format nil "No image ~a is visible to this account." id)
        :errorp t))
      (t
       (tool-result
        (encode-json-to-string
         (obj "id" (util:oid image)
              "url" (image-url image))))))))

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

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
                #:*tools*
                #:call-tool
                #:register-tool
                #:tool
                #:tool-definitions
                #:tool-description
                #:tool-name
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
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/model/report
                #:report)
  (:import-from #:screenshotbot/model/screenshot
                #:make-screenshot)
  (:import-from #:util/store/object-id
                #:oid)
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
      (let ((text (progv (list 'screenshotbot/mcp/channels::+max-channels+)
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
;; fetch_report
;; ----------------------------------------------------------------------

(defun fetch-report-as (token id)
  (tool-text
   (post-as token
            :json (format nil "{\"jsonrpc\":\"2.0\",\"id\":9,~
\"method\":\"tools/call\",\"params\":{\"name\":\"fetch_report\",~
\"arguments\":{\"report_id\":~s}}}" id))))

(test fetching-an-unknown-report-is-a-tool-error-not-a-crash
  "A model hands us whatever id it has. A malformed one has to come back
as something it can read and correct."
  (with-fixture caller ()
    (dolist (id (list "not-an-oid" "" "000000000000000000000000"))
      (multiple-value-bind (text result) (fetch-report-as token id)
        (declare (ignore text))
        (is-true (field result "isError")
                 "id ~s did not produce a tool error" id)))))

(test an-unknown-and-a-forbidden-report-are-indistinguishable
  "Otherwise a caller could enumerate which report ids exist by watching
the error change."
  (with-fixture caller ()
    (multiple-value-bind (missing) (fetch-report-as token "000000000000000000000000")
      (multiple-value-bind (malformed) (fetch-report-as token "111111111111111111111111")
        ;; Same shape of answer, differing only in the id echoed back.
        (is (equal (str:replace-all "000000000000000000000000" "ID" missing)
                   (str:replace-all "111111111111111111111111" "ID" malformed)))))))

(test fetching-a-report-without-the-scope-never-reaches-it
  (with-fixture caller ()
    (multiple-value-bind (body status)
        (post-as (token-with '("profile"))
                 :json "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/call\",~
\"params\":{\"name\":\"fetch_report\",\"arguments\":{\"report_id\":\"x\"}}}")
      (declare (ignore body))
      (is (equal 403 status)))))

(defun static-asset (file)
  (path:catfile
   (asdf:system-relative-pathname :screenshotbot "static/")
   file))

(defun make-changed-report (company channel)
  "A report whose run changed one screenshot relative to its previous run."
  ;; :COMPANY matters -- AUTH:CAN-VIEWER-VIEW on an image defers to its
  ;; company, so an image without one is visible to nobody.
  (let* ((before-image (make-image :company company
                                   :pathname
                                   (static-asset "assets/images/example-view.svg.png")))
         (after-image (make-image :company company
                                  :pathname
                                  (static-asset "assets/images/example-view-square.svg.png")))
         (previous (make-recorder-run
                    :company company :channel channel
                    :screenshots (list (make-screenshot :name "home"
                                                        :image before-image))))
         (run (make-recorder-run
               :company company :channel channel
               :screenshots (list (make-screenshot :name "home"
                                                   :image after-image)))))
    (values (make-instance 'report
                           :run run
                           :previous-run previous
                           :channel channel
                           :title "1 change")
            before-image
            after-image)))

(test a-report-describes-what-changed-with-image-ids
  "The point of the tool: enough for a model to ask for the two images and
see the difference itself."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (multiple-value-bind (report before-image after-image)
          (make-changed-report company channel)
        (multiple-value-bind (text result)
            (fetch-report-as token (oid report))
          (is-false (field result "isError"))
          (let* ((json (let ((json:*json-identifier-name-to-lisp* #'identity)
                             (json:*identifier-name-to-key* #'identity))
                         (json:decode-json-from-string text)))
                 (changed (field json "changed")))
            (is (equal (oid report) (field json "id")))
            (is (equal "1 change" (field json "title")))
            (is (equal "web" (field json "channel")))
            (is-true (str:containsp "/report/" (field json "url")))
            (is (equal 1 (length changed)))
            (is (equal "home" (field (first changed) "name")))
            ;; The ids a model then feeds to fetch_image_url, and they are
            ;; the right way round -- before is the previous run's.
            (is (equal (oid before-image)
                       (field (field (first changed) "before") "imageId")))
            (is (equal (oid after-image)
                       (field (field (first changed) "after") "imageId")))))))))

(test a-report-belonging-to-another-account-is-not-readable
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (channel (make-instance 'channel :name "theirs" :company other)))
      (multiple-value-bind (report) (make-changed-report other channel)
        (multiple-value-bind (text result) (fetch-report-as token (oid report))
          (declare (ignore text))
          (is-true (field result "isError")))))))

;; ----------------------------------------------------------------------
;; fetch_image_url
;; ----------------------------------------------------------------------

(defun fetch-image-url-as (token id)
  (tool-text
   (post-as token
            :json (format nil "{\"jsonrpc\":\"2.0\",\"id\":9,~
\"method\":\"tools/call\",\"params\":{\"name\":\"fetch_image_url\",~
\"arguments\":{\"image_id\":~s}}}" id))))

(test an-image-id-resolves-to-an-absolute-url
  "IMAGE-PUBLIC-URL can return a site-relative path, which is useless to a
model on the other side of the internet."
  (with-fixture caller ()
    (let ((image (make-image :company company
                             :pathname
                             (static-asset "assets/images/example-view.svg.png"))))
      (multiple-value-bind (text result) (fetch-image-url-as token (oid image))
        (is-false (field result "isError"))
        (let* ((json (let ((json:*json-identifier-name-to-lisp* #'identity)
                           (json:*identifier-name-to-key* #'identity))
                       (json:decode-json-from-string text)))
               (url (field json "url")))
          (is (equal (oid image) (field json "id")))
          (is-true url)
          (is-true (str:starts-with-p "http" url)))))))

(test the-ids-a-report-hands-out-are-the-ids-this-tool-accepts
  "The two tools are only useful composed, and nothing else checks that
one's output is the other's input."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (multiple-value-bind (report) (make-changed-report company channel)
        (let* ((report-json
                 (let ((json:*json-identifier-name-to-lisp* #'identity)
                       (json:*identifier-name-to-key* #'identity))
                   (json:decode-json-from-string
                    (fetch-report-as token (oid report)))))
               (change (first (field report-json "changed"))))
          (dolist (side (list "before" "after"))
            (let ((image-id (field (field change side) "imageId")))
              (is-true image-id "~a had no imageId" side)
              (multiple-value-bind (text result) (fetch-image-url-as token image-id)
                (is-false (field result "isError")
                          "~a image id ~a did not resolve" side image-id)
                (is-true (str:containsp "http" text))))))))))

(test an-unknown-or-malformed-image-id-is-a-tool-error
  (with-fixture caller ()
    (dolist (id (list "not-an-oid" "" "000000000000000000000000"))
      (multiple-value-bind (text result) (fetch-image-url-as token id)
        (declare (ignore text))
        (is-true (field result "isError")
                 "id ~s did not produce a tool error" id)))))

(test an-image-belonging-to-another-account-is-not-resolvable
  "Otherwise an image id leaked from anywhere would resolve to a URL."
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (image (make-image :company other
                              :pathname (static-asset
                                         "assets/images/example-view.svg.png"))))
      (multiple-value-bind (text result) (fetch-image-url-as token (oid image))
        (declare (ignore text))
        (is-true (field result "isError"))))))

;; ----------------------------------------------------------------------
;; DEF-TOOL and the registry
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
  "This file is reloaded into a running image. A registry that appended
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

(test a-missing-required-argument-is-refused-by-name
  "DEF-TOOL generates the check, so the body never sees a blank argument.
Naming it is what lets a model fix its own call."
  (with-fixture caller ()
    (multiple-value-bind (text result) (fetch-report-as token "")
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

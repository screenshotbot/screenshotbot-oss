;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/test-util
  (:use #:cl
        #:fiveam)
  (:import-from #:core/installation/installation
                #:abstract-installation
                #:*installation*)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:screenshotbot/mcp/mcp
                #:mcp-handler)
  (:import-from #:screenshotbot/api/core
                #:authenticate-api-request
                #:authenticate-request-from-key
                #:bearer-token)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:screenshotbot/model/company
                #:company-channels)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/model/report
                #:report)
  (:import-from #:screenshotbot/model/screenshot
                #:make-screenshot)
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
  (:export
   ;; The harness
   #:caller
   #:post-as
   #:call-tool-as
   #:tool-text
   #:decode
   #:field
   #:static-asset
   #:make-changed-report
   ;; Names the CALLER fixture binds. A FiveAM fixture splices the test
   ;; body into its own bindings, so a test in another package can only
   ;; see them if it refers to these very symbols -- hence the exports.
   #:company
   #:token
   #:token-with
   #:add-channel)
  (:documentation "Shared harness for the /mcp tests.

The tests split by subject -- protocol, channels, reports, images -- but
they all need the same three things: a store with a company in it, a
bearer token scoped for the endpoint, and a way to put a JSON-RPC call
through the real handler."))
(in-package :screenshotbot/mcp/test-util)

(defun decode (json)
  "Decode JSON with member names left exactly as they arrive on the wire.

CL-JSON's reader would otherwise turn `inputSchema' into :INPUT-SCHEMA,
which hides precisely the class of bug these tests exist to catch: we
shipped `protocolversion' and `serverinfo' for months and every test
passed."
  (let ((json:*json-identifier-name-to-lisp* #'identity)
        (json:*identifier-name-to-key* #'identity))
    (json:decode-json-from-string json)))

(defun field (object name)
  (assoc-value object name :test #'equal))

;; ----------------------------------------------------------------------
;; A caller
;; ----------------------------------------------------------------------

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

;; ----------------------------------------------------------------------
;; Putting a call through the handler
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

(defun call-tool-as (api-key name &optional arguments)
  "Call tool NAME as API-KEY. ARGUMENTS is an alist of JSON name to string."
  (post-as api-key
           :json (format nil "{\"jsonrpc\":\"2.0\",\"id\":9,~
\"method\":\"tools/call\",\"params\":{\"name\":~s,\"arguments\":{~{~s:~s~^,~}}}}"
                         name
                         (loop for (key . value) in arguments
                               collect key
                               collect value))))

(defun tool-text (body)
  "The text content out of a tools/call result."
  (let* ((response (decode body))
         (result (field response "result")))
    (values (field (first (field result "content")) "text")
            result
            response)))

;; ----------------------------------------------------------------------
;; Model setup
;; ----------------------------------------------------------------------

(defun static-asset (file)
  (path:catfile
   (asdf:system-relative-pathname :screenshotbot "static/")
   file))

(defun make-changed-report (company channel)
  "A report whose run changed one screenshot relative to its previous run.

Here rather than next to the fetch_report tests because the image tests
need it too: the only way to check that the ids one tool hands out are
the ids the other accepts is to have a real report to ask."
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

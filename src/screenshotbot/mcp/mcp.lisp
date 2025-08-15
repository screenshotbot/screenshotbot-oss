;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/mcp
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:json
                #:encode-json-to-string))
(in-package :screenshotbot/mcp/mcp)

(defun list-tools (id)
  (encode-json-to-string
   `((:jsonrpc . "2.0")
     (:id . ,id)
     (:result . ((:tools . (((:name . "list_channels")
                             (:description . "List all channels (projects) in Screenshotbot")
                             (:inputSchema . ((:type . "object")
                                              (:properties . ())
                                              (:required . ())))))))))))

(defun list-resources (id)
  (encode-json-to-string
   `((:jsonrpc . "2.0")
     (:id . ,id)
     (:result . ((:resources . (((:uri . "channel://list")
                                 (:name . "channels")
                                 (:description . "List of all channels (projects) in Screenshotbot")
                                 (:mimeType . "application/json")))))))))

(defhandler (nil :uri "/mcp" :method :post) ()
  (setf (hunchentoot:header-out :content-type) "application/json")
  (let* ((request-body (hunchentoot:raw-post-data :force-text t))
         (request-json (when request-body
                         (cl-json:decode-json-from-string request-body)))
         (method (cdr (assoc :method request-json)))
         (id (cdr (assoc :id request-json))))
    (log:info "Got body: ~a" request-body)
    (cond
      ((string= method "initialize")
       (encode-json-to-string
         `((:jsonrpc . "2.0")
           (:id . ,id)
           (:result . 
             ((:protocolVersion . "2024-11-05")
              (:capabilities . 
                ((:tools . ((:listChanged . nil)))
                 (:resources . ((:subscribe . nil)
                               (:listChanged . nil)))
                 (:prompts . ((:listChanged . nil)))
                 (:logging . nil)))
              (:serverInfo . 
                ((:name . "Screenshotbot MCP Server")
                 (:version . "1.0.0"))))))))
      
      ((string= method "tools/list")
       (list-tools id))
      
      ((string= method "resources/list")
       (list-resources id))
      
      (t
       (encode-json-to-string
         `((:jsonrpc . "2.0")
           (:id . ,id)
           (:error . ((:code . -32601)
                     (:message . "Method not found")))))))))



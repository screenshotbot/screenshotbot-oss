;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/config
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/settings-api
                #:defsettings
                #:settings-template))
(in-package :screenshotbot/mcp/config)

(named-readtables:in-readtable markup:syntax)

(defun mcp-settings ()
  <settings-template>
    <div class= "card">
      <div class= "card-header">
        <h3>MCP server</h3>
      </div>
      <div class= "card-body">
        <div class="mb-2">
          <label for="url" class= "form-label" >MCP endpoint</label>
          <input type= "text" disabled= "disabled" value= (hex:make-full-url hunchentoot:*request* "/mcp") class= "form-control" />
          <div class= "mt-1 text-muted"> Use this in Claude Web, Claude Code, Codex etc. Your agent will authenticate using OAuth.</div>
        </div>
      </div>
    </div>
  </settings-template>)

(defsettings mcp
  :name "mcp"
  :title "MCP"
  :section nil
  :handler #'mcp-settings)



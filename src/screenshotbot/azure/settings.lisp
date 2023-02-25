;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/azure/settings
  (:use #:cl)
  (:import-from #:screenshotbot/azure/plugin
                #:azure-plugin)
  (:import-from #:screenshotbot/settings-api
                #:settings-template
                #:defsettings)
  (:import-from #:nibble
                #:nibble))
(in-package :screenshotbot/azure/settings)

(named-readtables:in-readtable markup:syntax)

(defun azure-settings-page ()
  (let ((submit (nibble ())))
    <settings-template>
      <form action=submit method= "POST">
        <div class= "card mt-3">
          <div class= "card-header">
            <h3>Azure DevOps Integration</h3>
          </div>

          <div class= "card-body">
            <div class= "mb-3">
              <label for= "server" class= "form-label" >
                Server URL
              </label>

              <input type= "text" class= "form-control" name= "server"
                     placeholder= "https://dev.azure.com" />
            </div>

            <div class= "mb-3">
              <label for= "access-token" class= "form-label">
                Personal Access Token
              </label>

              <input type= "password" class= "form-control" name= "server"
                     placeholder= "*****" />
            </div>
          </div>

          <div class= "card-footer">
            <input type= "submit" class= "btn btn-primary" value= "Save" />
          </div>
        </div>
      </form>
    </settings-template>))

(defsettings azure-settings
  :name "azure"
  :title "Azure DevOps"
  :section :vcs
  :plugin 'azure-plugin
  :handler 'azure-settings-page)

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
                #:nibble)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util/form-errors
                #:with-error-builder)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:bknr.datastore
                #:delete-object))
(in-package :screenshotbot/azure/settings)

(named-readtables:in-readtable markup:syntax)

(with-class-validation
  (defclass azure-settings (store-object)
    ((%server :initarg :server
              :reader azure-server)
     (%personal-access-token
      :initarg :access-token
      :reader azure-access-token)
     (%company :reader company
               :index-type unique-index
               :index-reader azure-settings-for-company
               :initarg :company))
    (:metaclass persistent-class)))

(defun finalize-settings (server access-token)
  (let ((existing (azure-settings-for-company (current-company))))
    (when existing
      (delete-object existing))
    (make-instance 'azure-settings
                   :company (current-company)
                   :access-token access-token
                   :server server))
  (hex:safe-redirect "/settings/azure"))

(defun save-settings (server access-token)
  (with-error-builder (:check check
                       :success (finalize-settings server
                                                   access-token)
                       :form-builder (azure-settings-page)
                       :form-args (:server server
                                   :access-token access-token)
                       :errors errors)
    (check :server (not (str:emptyp server))
           "Server should not be empty")
    (check :server (ignore-errors (quri:uri server))
           "Could not parse URI provided")
    (check :access-token (not (str:emptyp access-token))
           "Personal access token cannot be empty")))

(defun azure-settings-page ()
  (let* ((settings (azure-settings-for-company (current-company)))
         (existing-token (?. azure-access-token settings))
         (submit (nibble (server access-token)
                   (save-settings server
                                  (cond
                                    ((equal "unchanged" access-token)
                                     existing-token)
                                    (t
                                     access-token))))))
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
                     value= (?. azure-server settings)
                     placeholder= "https://dev.azure.com" />
            </div>

            <div class= "mb-3">
              <label for= "access-token" class= "form-label">
                Personal Access Token
              </label>

              <input type= "password" class= "form-control" name= "access-token"
                     value= (when settings "unchanged")
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

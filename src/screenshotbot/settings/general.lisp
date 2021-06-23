;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/settings/general
    (:use #:cl
          #:alexandria
          #:markup
          #:../model/user
          #:../user-api
          #:nibble
          #:./settings-template)
  (:import-from #:bknr.datastore
                #:with-transaction))

(markup:enable-reader)

(defun %general-settings ()
  <settings-template>
  <div class= "card mt-3">
  <div class= "card-header">
        <h4>Account Details</h4>
      </div>
      <div class= "card-body">
          <div class= "row">
            <div class= "col-md-6">
              <div class= "setting-item-title" >Name</div>
              <div class= "setting-item-subtitle" >Your full name</div>
            </div>
            <div class= "col-md-6">
              <div class= "form-group setting-form-group">
                <div class= "row">
                  <input type= "text" name= "name" class= "form-control setting-auto-update"
                         data-update-url= "/api/settings/update-name"
                         value= (user-full-name (current-user)) />
                  <div class= "invalid-feedback"/>
                </div>
              </div>
            </div>
          </div>
      </div>
    </div>
    </settings-template>)

(defsettings general-settings
  :name "general"
  :title "General"
  :section nil
  :handler '%general-settings)

(defapi (nil :uri "/api/settings/update-name" :method :post) (name)
  (when (str:emptyp name)
    (error 'api-error
           :message
           "Can't use an empty name"))
  (with-transaction ()
    (setf (user-full-name (current-user))
          name)))

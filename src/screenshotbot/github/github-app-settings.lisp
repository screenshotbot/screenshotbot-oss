;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/github-app-settings
  (:use #:cl)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:export
   #:github-app-settings-form))
(in-package :screenshotbot/github/github-app-settings)

(named-readtables:in-readtable markup:syntax) 

(defun github-app-settings-form (github-app)
  <simple-card-page>
    <div class= "card-header">
      <h3>Update GitHub app id</h3>
    </div>
    <div class= "mb-3">
      <label for= "app-id" class= "form-label" >GitHub App ID</label>
      <input type= "text" name= "app-id" id= "app-id" class= "form-control" placeholder= "1234" />
    </div>

    <div class= "mb-3">
      <label for= "private-key" class= "form-label" >Private Key (copy from pem file) </label>
      <textarea class= "form-control" name= "private-key" id= "private-key" />
    </div>
    <div class= "card-footer">
      <input type= "submit" value= "Update" class= "btn btn-primary" />
      <a href= "/settings/github" class= "btn btn-secondary">Cancel</a>
    </div>
  </simple-card-page>)


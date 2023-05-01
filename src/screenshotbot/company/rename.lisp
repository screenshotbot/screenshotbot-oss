;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/company/rename
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:settings-template
                #:defsettings)
  (:import-from #:screenshotbot/model/company
                #:company-with-name
                #:company-admin-p)
  (:import-from #:screenshotbot/user-api
                #:personalp
                #:company-name
                #:current-company
                #:current-user)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:util/form-errors
                #:with-error-builder
                #:with-form-errors)
  (:import-from #:bknr.datastore
                #:with-transaction))
(in-package :screenshotbot/company/rename)

(named-readtables:in-readtable markup:syntax)

(defsettings  org-general
  :name "organization"
  :section :organization
  :title "General"
  :handler 'general-company-page)

(defun general-company-page (&key (company (current-company))
                          success)
  (let ((post (nibble (name :name :company-name-change-post)
                (%post :company company
                       :name name))))
    <settings-template>
      <div class= "alert alert-danger d-none mt-3">
      </div>

      ,(when success
         <div class= "alert alert-success mt-3">
           Organization updated.
         </div>)

      <form action=post method= "POST">
        <div class= "card mt-3" >
          <div class= "card-header">
            <h3>Change organization name</h3>
          </div>
          <div class= "card-body">
            <div class= "form-group">
              <label for= "name" class= "form-label" >Updated name</label>
              <input type= "text" value= (company-name company) class= "form-control"
                     name= "name"
                     id= "name" />
            </div>
          </div>

          <div class= "card-footer">
            <input type= "submit" class= "btn btn-primary" value= "Change" />
          </div>
        </div>
      </form>
    </settings-template>))

(defun %post (&key company name)
  (with-error-builder (:check check
                       :errors errors
                       :form-builder (general-company-page :company company)
                       :form-args (:name name)
                       :success (%finish-post :company company
                                              :name name))
    (check :name (not (company-with-name name))
           "This company name is already in use")
    (check :name (>= (length name) 6)
           "Company name must be at least 6 letters long")
    (check nil (company-admin-p company (current-user))
           "You must be an admin on this organization to change this page")
    (check nil (not (personalp company))
           "This organizaton is a legacy personal organization, and its name can't be changed.")))

(defun %finish-post (&key company name)
  (with-transaction ()
    (setf (company-name company)
          name))
  (hex:safe-redirect
   (nibble (:name :success-name-change)
     (general-company-page :company company :success t))))

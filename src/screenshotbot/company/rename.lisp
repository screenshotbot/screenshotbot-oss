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
                #:company-invitation-role
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
                #:with-transaction)
  (:import-from #:alexandria
                #:assoc-value)
  (:local-nicknames (#:roles #:auth/model/roles)))
(in-package :screenshotbot/company/rename)

(named-readtables:in-readtable markup:syntax)

(defsettings  org-general
  :name "organization"
  :section :organization
  :title "General"
  :handler 'general-company-page)

(defparameter *roles*
  `(("site-admin" "Do not allow invitations" roles:site-admin)
    ("admin" "Only Admins" roles:admin)
    ("member" "Any member (excluding guests)" roles:standard-member)))

(defun general-company-page (&key (company (current-company))
                          success)
  (let ((post (nibble (name invitation-role :name :company-name-change-post)
                (%post :company company
                       :invitation-role invitation-role
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
            <h3>Organization settings</h3>
          </div>
          <div class= "card-body">
            <div class= "form-group">
              <label for= "name" class= "form-label" >Organization name</label>
              <input type= "text" value= (company-name company) class= "form-control"
                     name= "name"
                     id= "name" />
            </div>

            <div class= "form-group mt-2">
              <label for= "invitation-role" class= "form-label">Who can invite other members?</label>
              <select name= "invitation-role" id= "invitation-role" class= "form-select" >
                ,@ (loop for (type desc role) in *roles*
                         collect
                         <option value= type selected= (when (eql role (company-invitation-role company)) "selected") >,(progn desc)</option>)
              </select>
            </div>
          </div>

          <div class= "card-footer">
            <input type= "submit" class= "btn btn-primary" value= "Change" />
          </div>
        </div>
      </form>
    </settings-template>))

(defun %post (&key company name invitation-role)
  (let ((invitation-role-sym
          (second (assoc-value *roles* invitation-role :test #'equal))))
   (with-error-builder (:check check
                        :errors errors
                        :form-builder (general-company-page :company company)
                        :form-args (:name name :invitation-role invitation-role)
                        :success (%finish-post :company company
                                               :invitation-role invitation-role-sym
                                               :name name))
     (check :name
            (or
             (equal name (company-name company))
             (not (company-with-name name)))
            "This company name is already in use")
     (check :invitation-role
            invitation-role-sym
            "Invalid invitation role")
     (check :name (>= (length name) 6)
            "Company name must be at least 6 letters long")
     (check nil (company-admin-p company (current-user))
            "You must be an admin on this organization to change this page")
     (check nil (not (personalp company))
            "This organizaton is a legacy personal organization, and its name can't be changed."))))

(defun %finish-post (&key company name invitation-role)
  (with-transaction ()
    (setf (company-name company)
          name)
    (setf (company-invitation-role company)
          invitation-role))
  (hex:safe-redirect
   (nibble (:name :success-name-change)
     (general-company-page :company company :success t))))

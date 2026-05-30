;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/saml/settings
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:core/ui/taskie
                #:taskie-page-title
                #:taskie-list)
  (:import-from #:screenshotbot/login/saml
                #:saml-entity-id
                #:saml-auth-providers-for-company)
  (:import-from #:screenshotbot/login/common
                #:with-login)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:util/form-errors
                #:with-error-builder
                #:with-form-errors))
(in-package :screenshotbot/saml/settings)

(named-readtables:in-readtable markup:syntax)

(defun check-enabled! ()
  (assert (gk:check :self-service-saml (auth:current-company))))

(defhandler (nil :uri "/sso") ()
  "List all the SAML configs"
  (check-enabled!)
  (with-login ()
    <app-template>
      <taskie-page-title title= "Configure Single-Sign-on">
        <a href= (nibble () (%new-sso)) class= "btn btn-primary mb-2" >Create</a>
      </taskie-page-title>

      <table class= "table border mt-3">
        <thead>
          <th>Type</th>
          <th>IdP Entity ID</th>
          <th>Actions</th>
        </thead>
        ,@(loop for saml in (fset:convert 'list (saml-auth-providers-for-company (auth:current-company)))
                collect
                <tr class= "align-middle" >
                  <td>SAML</td>
                  <td><tt>,(saml-entity-id saml)</tt></td>
                  <td>
                    <a href= "#" class= "btn btn-sm btn-secondary" >Edit</a>
                    <a href= "#" class= "btn btn-sm btn-danger" >Delete</a>
                  </td>
                </tr>)
      </table>
    </app-template>))

(defun sso-form ()
  (let ((action (nibble (name idp-metadata)
                  (submit-sso-form :name name :idp-metadata idp-metadata))))
    <simple-card-page form-action=action >
      <div class= "card-header">
        <h4 class= "card-title">
          New SAML connection
        </h4>
      </div>

      <div class= "mb-2" >
        <label for= "name" class= "form-label">Friendly Name</label>
        <div class= "text-muted">
          This will only be used to distinguish between different authentication methods
        </div>
        <input type= "text" id= "name" name= "name" class= "form-control"  />
      </div>
      
      <div class= "mb-2" >
        <label for= "sp-metadata" class= "form-label">Service Provider EntityID / Metadata URL</label>
        <div class= "text-muted">
          Copy this into your Identity Provider
        </div>      
        <input type= "text" id= "sp-metadata" disabled= "disabled" class= "form-control"  value= (hex:make-full-url hunchentoot:*request* "/saml/metadata") />
      </div>

      <div class= "mb-2" >
        <label for= "idp-metadata" class= "form-label">Identity Provider Metadata URL</label>
        <input type= "text" name= "idp-metadata" id= "idp-metadata" class= "form-control"   placeholder= "https://idp.example.com/saml/metadata" />
      </div>
      
      <div class= "card-footer">
        <p class= "text-muted">
          You will have an opportunity to test the connection after you create it
        </p>
        <input type= "submit" class= "btn btn-primary" value= "Create" />
      </div>
    </simple-card-page>))

(defun submit-sso-form (&key name idp-metadata)
  (with-error-builder (:errors errors
                       :check check
                       :form-builder (sso-form)
                       :form-args (:name name :idp-metadata idp-metadata)
                       :success (error "Unimpl"))
    (check :name
           (str:non-blank-string-p name)
           "Name must not be empty")))

(defun %new-sso ()
  (sso-form))





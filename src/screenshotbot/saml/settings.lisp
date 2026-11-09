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
                #:saml-auth-provider
                #:saml-company
                #:fetch-metadata-xml
                #:saml-entity-id
                #:saml-auth-providers-for-company
                #:test-signin-link
                #:last-saml-test-result
                #:saml-test-result-email
                #:saml-test-result-error
                #:defaultp
                #:activation-blocker
                #:activate-saml-provider
                #:deactivate-saml-provider)
  (:import-from #:screenshotbot/login/common
                #:with-login)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:util/form-errors
                #:with-error-builder
                #:with-form-errors)
  (:import-from #:util/store/object-id
                #:find-by-oid
                #:oid)
  (:import-from #:screenshotbot/model/company
                #:company
                #:company-admin-p))
(in-package :screenshotbot/saml/settings)

(named-readtables:in-readtable markup:syntax)

(defun check-enabled! (company)
  (assert (gk:check :self-service-saml company)))

(defun check-admin! (company)
  "Only admins may see or change SAML settings: a SAML provider can log
in as any email address."
  (unless (company-admin-p company (auth:current-user))
    (error "You must be an admin to configure SSO")))

(defun sso-page-url (company)
  (hex:make-url "/company/:oid/sso" :oid (oid company)))

(defhandler (nil :uri "/sso") ()
  (with-login ()
    (hex:safe-redirect (sso-page-url (auth:current-company)))))

(defhandler (company-sso-page :uri "/company/:oid/sso") (oid)
  "List all the SAML configs for the company"
  (with-login ()
    (let ((company (find-by-oid oid)))
      (check-type company company)
      (auth:can-view! company)
      (check-enabled! company)
      (check-admin! company)
      (sso-page company))))

(defun sso-page (company)
  <app-template>
    <taskie-page-title title= "Configure Single-Sign-on">
      <a href= (nibble () (sso-form company)) class= "btn btn-primary mb-2" >Create</a>
    </taskie-page-title>

    <table class= "table border mt-3">
      <thead>
        <th>Type</th>
        <th>IdP Entity ID</th>
        <th>Last test</th>
        <th>Status</th>
        <th>Actions</th>
      </thead>
      ,@(loop for saml in (fset:convert 'list (saml-auth-providers-for-company company))
              collect
              (util:copying (saml)
              <tr class= "align-middle" >
                <td>SAML</td>
                <td><tt>,(saml-entity-id saml)</tt></td>
                <td><test-result result= (last-saml-test-result saml) /></td>
                <td><activation-status saml=saml /></td>
                <td>
                  <a href= (nibble () (start-test-login saml)) class= "btn btn-sm btn-primary" >Test</a>
                  <activation-button saml=saml />
                  <a href= "#" class= "btn btn-sm btn-secondary" >Edit</a>
                  <a href= "#" class= "btn btn-sm btn-danger" >Delete</a>
                </td>
              </tr>))
    </table>
  </app-template>)

(markup:deftag test-result (&key result)
  (cond
    ((null result)
     <span class= "text-muted">Never tested</span>)
    ((saml-test-result-error result)
     <span class= "text-danger">Failed: ,(saml-test-result-error result)</span>)
    (t
     <span class= "text-success">Signed in as ,(saml-test-result-email result)</span>)))

(markup:deftag activation-status (&key saml)
  (cond
    ((defaultp saml)
     <span class= "badge bg-success">Active</span>)
    (t
     <span class= "text-muted">
       ,(or (activation-blocker saml (auth:current-user))
            "Ready to activate")
     </span>)))

(markup:deftag activation-button (&key saml)
  (cond
    ((defaultp saml)
     ;; TODO: a proper confirmation page, explaining what happens to
     ;; users who don't have another way to log in.
     <a href= (nibble () (%deactivate saml)) class= "btn btn-sm btn-warning"
        onclick= "return confirm('Deactivate SAML? Users will no longer be able to sign in with it.')" >Deactivate</a>)
    ((activation-blocker saml (auth:current-user))
     <button class= "btn btn-sm btn-success" disabled= "disabled" >Activate</button>)
    (t
     <a href= (nibble () (%activate saml)) class= "btn btn-sm btn-success" >Activate</a>)))

(defun check-provider! (saml)
  "Nibbles re-check access, since admin rights could have changed since
the page was rendered."
  (let ((company (saml-company saml)))
    (check-enabled! company)
    (check-admin! company)))

(defun go-back (company)
  (hex:safe-redirect (sso-page-url company)))

(defun %activate (saml)
  (check-provider! saml)
  (activate-saml-provider saml (auth:current-user))
  (go-back (saml-company saml)))

(defun %deactivate (saml)
  (check-provider! saml)
  (deactivate-saml-provider saml)
  (go-back (saml-company saml)))

(defun start-test-login (saml)
  (check-provider! saml)
  ;; TODO: if the IdP metadata can't be loaded, this signals
  ;; INVALID-METADATA-URL and the admin gets a 500. Record it as a
  ;; failed test instead.
  (hex:safe-redirect (test-signin-link saml
                                       (auth:current-user)
                                       (sso-page-url (saml-company saml)))))

(defun sso-form (company)
  (let ((action (nibble (name idp-metadata)
                  (submit-sso-form company :name name :idp-metadata idp-metadata))))
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
        <input type= "text" id= "sp-metadata" disabled= "disabled" class= "form-control"  value= (hex:make-full-url hunchentoot:*request* "/saml/entity/:oid/metadata" :oid (oid company)) />
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

(defun submit-sso-form (company &rest args &key name idp-metadata)
  (check-enabled! company)
  (check-admin! company)
  (with-error-builder (:errors errors
                       :check check
                       :form-builder (sso-form company)
                       :form-args (:name name :idp-metadata idp-metadata)
                       :success (apply #'%create-saml company args))
    (check :name
           (str:non-blank-string-p name)
           "Name must not be empty")
    (check :idp-metadata
           (str:non-blank-string-p idp-metadata)
           "IdP Metadata URL must be provided. Please contact us if you need to provide an XML directly.")
    (check
     :idp-metadata
     (ignore-errors (quri:uri idp-metadata))
     "Invalid URI")
    (check
     :idp-metadata
     (ignore-errors (fetch-metadata-xml idp-metadata))
     "Could not fetch metadata URL, if this is behind an internal network please contact support@screenshotbot.io to help set it up")))

(auto-restart:with-auto-restart ()
 (defun %create-saml (company &key name idp-metadata)
   (make-instance 'saml-auth-provider
                  :company company
                  :idp-metadata-url idp-metadata
                  :name name)
   (go-back company)))

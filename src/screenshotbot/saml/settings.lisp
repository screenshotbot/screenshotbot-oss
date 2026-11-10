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
                #:deactivate-saml-provider
                #:edit-saml-provider
                #:saml-idp-metadata-url
                #:saml-name)
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
                #:company-admin-p)
  (:import-from #:screenshotbot/login/recovery-token
                #:recovery-token
                #:find-recovery-token
                #:recovery-token-company
                #:recovery-token-expires-at
                #:recovery-token-expired-p))
(in-package :screenshotbot/saml/settings)

(named-readtables:in-readtable markup:syntax)

(defun check-enabled! (company)
  (assert (gk:check :self-service-saml company)))

(defparameter *recovery-session-duration* 3600
  "How long a session can use a recovery token after redeeming it. The
token itself may be valid for longer, in which case it can be redeemed
again.")

(defun session-recovery-token (company)
  "The recovery token redeemed in this session, if it's for COMPANY and
neither the token nor this session's use of it has expired. Returns the
time the session's access ends as a second value.

This is the only place that reads it, so it can't give access to
anything but this page."
  (destructuring-bind (&optional token deadline)
      (auth:session-value :recovery-token)
    ;; Check the deadline ourselves: SESSION-VALUE doesn't enforce
    ;; :EXPIRES-IN when reading.
    (when (and (typep token 'recovery-token)
               (eql company (recovery-token-company token))
               (not (recovery-token-expired-p token))
               (integerp deadline)
               (< (get-universal-time) deadline))
      (values token deadline))))

(defun sso-principal (company)
  "Who is making changes on COMPANY's SSO page: a company admin, or a
recovery token. NIL if neither."
  (let ((user (auth:current-user)))
    (cond
      ((and user (company-admin-p company user))
       user)
      (t
       (session-recovery-token company)))))

(defun check-principal! (company)
  "Only admins (or a recovery token) may see or change SAML settings: a
SAML provider can log in as any email address."
  (check-enabled! company)
  (or (sso-principal company)
      (error "You must be an admin to configure SSO")))

(defun sso-page-url (company)
  (hex:make-url "/company/:oid/sso" :oid (oid company)))

(defhandler (nil :uri "/sso") ()
  (with-login ()
    (hex:safe-redirect (sso-page-url (auth:current-company)))))

(defhandler (company-sso-page :uri "/company/:oid/sso") (oid)
  "List all the SAML configs for the company"
  (let ((company (find-by-oid oid)))
    (check-type company company)
    (cond
      ((session-recovery-token company)
       (check-principal! company)
       (sso-page company))
      (t
       (with-login ()
         (auth:can-view! company)
         (check-principal! company)
         (sso-page company))))))

(defhandler (recovery-page :uri "/recovery") ()
  (recovery-form))

(defun recovery-form (&key error-message)
  ;; The token is POSTed, so it doesn't end up in access logs.
  (let ((action (nibble (token)
                  (redeem-recovery-token token))))
    <simple-card-page form-action=action >
      <div class= "card-header">
        <h4 class= "card-title">Recover SSO access</h4>
      </div>
      ,(when error-message
         <div class= "alert alert-danger">,(progn error-message)</div>)
      <div class= "mb-2">
        <label for= "token" class= "form-label">Recovery token</label>
        <div class= "text-muted">
          Paste the token you got from Screenshotbot support. It only gives access to your SSO settings.
        </div>
        <input type= "password" id= "token" name= "token" class= "form-control" autocomplete= "off" />
      </div>
      <div class= "card-footer">
        <input type= "submit" class= "btn btn-primary" value= "Continue" />
      </div>
    </simple-card-page>))

(defun redeem-recovery-token (token)
  (let ((recovery-token (find-recovery-token token)))
    (cond
      ((null recovery-token)
       (recovery-form :error-message "This token is invalid or has expired."))
      (t
       (log:warn "Recovery token ~a redeemed for ~a"
                 (bknr.datastore:store-object-id recovery-token)
                 (recovery-token-company recovery-token))
       ;; New session, like logging in does (see MAYBE-RESET-SESSION),
       ;; so a session planted in the browser before this doesn't
       ;; get access.
       (unless (eql recovery-token (first (auth:session-value :recovery-token)))
         (auth:reset-session))
       ;; This deliberately doesn't log anyone in: the token is only
       ;; ever read by SESSION-RECOVERY-TOKEN.
       (let ((deadline (min (+ (get-universal-time) *recovery-session-duration*)
                            (recovery-token-expires-at recovery-token))))
         (setf (auth:session-value
                :recovery-token
                :expires-in (- deadline (get-universal-time)))
               (list recovery-token deadline)))
       (hex:safe-redirect (sso-page-url (recovery-token-company recovery-token)))))))

(markup:deftag recovery-token-banner (&key company)
  (multiple-value-bind (token deadline)
      (session-recovery-token company)
    (when token
      <div class= "alert alert-warning">
        You're using a recovery token. It only gives access to this page, until
        ,(local-time:format-rfc1123-timestring
          nil
          (local-time:universal-to-timestamp deadline)).
      </div>)))

(defun sso-page (company)
  <app-template>
    <taskie-page-title title= "Configure Single-Sign-on">
      <a href= (nibble () (sso-form company)) class= "btn btn-primary mb-2" >Create</a>
    </taskie-page-title>

    <recovery-token-banner company=company />

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
                  <a href= (nibble () (edit-sso-form saml)) class= "btn btn-sm btn-secondary" >Edit</a>
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
       ,(or (activation-blocker saml (sso-principal (saml-company saml)))
            "Ready to activate")
     </span>)))

(markup:deftag activation-button (&key saml)
  (cond
    ((defaultp saml)
     ;; TODO: a proper confirmation page, explaining what happens to
     ;; users who don't have another way to log in.
     <a href= (nibble () (%deactivate saml)) class= "btn btn-sm btn-warning"
        onclick= "return confirm('Deactivate SAML? Users will no longer be able to sign in with it.')" >Deactivate</a>)
    ((activation-blocker saml (sso-principal (saml-company saml)))
     <button class= "btn btn-sm btn-success" disabled= "disabled" >Activate</button>)
    (t
     <a href= (nibble () (%activate saml)) class= "btn btn-sm btn-success" >Activate</a>)))

(defun check-provider! (saml)
  "Nibbles re-check access, since admin rights could have changed (or the
recovery token expired) since the page was rendered. Returns the
principal."
  (check-principal! (saml-company saml)))

(defun go-back (company)
  (hex:safe-redirect (sso-page-url company)))

(defun %activate (saml)
  (activate-saml-provider saml (check-provider! saml))
  (go-back (saml-company saml)))

(defun %deactivate (saml)
  (check-provider! saml)
  (deactivate-saml-provider saml)
  (go-back (saml-company saml)))

(defun start-test-login (saml)
  ;; TODO: if the IdP metadata can't be loaded, this signals
  ;; INVALID-METADATA-URL and the admin gets a 500. Record it as a
  ;; failed test instead.
  (hex:safe-redirect (test-signin-link saml
                                       (check-provider! saml)
                                       (sso-page-url (saml-company saml)))))

(defun edit-sso-form (saml)
  (check-provider! saml)
  (sso-form (saml-company saml) :saml saml))

(defun sso-form (company &key saml)
  "The form to create a provider, or to edit SAML if it's given."
  (let ((action (nibble (name idp-metadata)
                  (submit-sso-form company :saml saml :name name :idp-metadata idp-metadata)))
        (url-locked-p (and saml (defaultp saml))))
    <simple-card-page form-action=action >
      <div class= "card-header">
        <h4 class= "card-title">
          ,(if saml "Edit SAML connection" "New SAML connection")
        </h4>
      </div>

      <div class= "mb-2" >
        <label for= "name" class= "form-label">Friendly Name</label>
        <div class= "text-muted">
          This will only be used to distinguish between different authentication methods
        </div>
        <input type= "text" id= "name" name= "name" class= "form-control"
               value= (when saml (saml-name saml)) />
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
        ,(when url-locked-p
           <div class= "text-muted">
             This connection is active, so its IdP can't be changed. To switch IdPs, create a new connection, test it and activate it.
           </div>)
        <input type= "text" name= "idp-metadata" id= "idp-metadata" class= "form-control"   placeholder= "https://idp.example.com/saml/metadata"
               value= (when saml (saml-idp-metadata-url saml))
               readonly= (when url-locked-p "readonly") />
      </div>
      
      <div class= "card-footer">
        <p class= "text-muted">
          ,(if saml
               "If you change the IdP, you'll need to test the connection again before activating it"
               "You will have an opportunity to test the connection after you create it")
        </p>
        <input type= "submit" class= "btn btn-primary" value= (if saml "Save" "Create") />
      </div>
    </simple-card-page>))

(defun submit-sso-form (company &key saml name idp-metadata)
  (check-principal! company)
  (when saml
    (assert (eql company (saml-company saml))))
  (with-error-builder (:errors errors
                       :check check
                       :form-builder (sso-form company :saml saml)
                       :form-args (:name name :idp-metadata idp-metadata)
                       :success (cond
                                  (saml
                                   (edit-saml-provider saml :name name :idp-metadata-url idp-metadata)
                                   (go-back company))
                                  (t
                                   (%create-saml company :name name :idp-metadata idp-metadata))))
    (check :idp-metadata
           (not (and saml
                     (defaultp saml)
                     (not (equal idp-metadata (saml-idp-metadata-url saml)))))
           "This connection is active, so its IdP can't be changed")
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

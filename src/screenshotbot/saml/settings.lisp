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
  (:import-from #:screenshotbot/settings-api
                #:defsettings
                #:should-show-settings-p
                #:settings-template)
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
                #:delete-saml-provider
                #:default-saml-auth-provider
                #:saml-idp-metadata-url
                #:saml-name
                #:entity-id
                #:callback-url)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:screenshotbot/login/common
                #:with-login)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:core/ui/simple-card-page
                #:confirmation-page
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
                #:recovery-token-expired-p)
  (:import-from #:auth/viewer-context
                #:viewer-context-company
                #:recovery-viewer-context)
  (:import-from #:screenshotbot/left-side-bar
                #:render-menu-items)
  (:import-from #:core/ui/left-side-bar
                #:left-nav-item)
  (:import-from #:screenshotbot/default-oidc-provider
                #:default-oidc-provider
                #:oidc-providers-for-company
                #:issuer
                #:oidc-provider-company
                #:client-id
                #:edit-default-oidc-provider
                #:activep
                #:active-default-oidc-provider
                #:activate-default-oidc-provider
                #:deactivate-default-oidc-provider
                #:delete-default-oidc-provider
                #:oidc-activation-blocker
                #:oidc-test-signin-link
                #:last-oidc-test-result
                #:oidc-test-result-email
                #:oidc-test-result-error)
  (:import-from #:oidc/oidc
                #:oidc
                #:discover)
  (:import-from #:screenshotbot/saml/login-all
                #:login-all-url)
  (:import-from #:core/ui/mdi
                #:mdi))
(in-package :screenshotbot/saml/settings)

(named-readtables:in-readtable markup:syntax)

(defun check-enabled! (company)
  (assert (gk:check :self-service-saml company)))

(defparameter *recovery-session-duration* 3600
  "How long a session can use a recovery token after redeeming it. The
token itself may be valid for longer, in which case it can be redeemed
again.")

(defun current-recovery-token ()
  "The recovery token redeemed in this session, if neither the token nor
this session's use of it has expired. Returns the time the session's
access ends as a second value."
  (destructuring-bind (&optional token deadline)
      (auth:session-value :recovery-token)
    ;; Check the deadline ourselves: SESSION-VALUE doesn't enforce
    ;; :EXPIRES-IN when reading.
    (when (and token
               (typep token 'recovery-token)
               (not (recovery-token-expired-p token))
               (integerp deadline)
               (< (get-universal-time) deadline))
      (values token deadline))))

(defun session-recovery-token (company)
  "Like CURRENT-RECOVERY-TOKEN, but only if the token is for COMPANY."
  (multiple-value-bind (token deadline) (current-recovery-token)
    (when (and token
               (eql company (recovery-token-company token)))
      (values token deadline))))

(defmethod auth:authenticate-request :after ((request auth:authenticated-request))
  "If nobody is logged in, but the session redeemed a recovery token, view
as a RECOVERY-VIEWER-CONTEXT for the token's company."
  (unless (auth:request-user request)
    (let ((token (current-recovery-token)))
      (when token
        (setf (auth:viewer-context request)
              (make-instance 'recovery-viewer-context
                             :company (recovery-token-company token)
                             :recovery-token token))))))

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

(markup:deftag recovery-token-banner (&key company)
  (multiple-value-bind (token deadline)
      (session-recovery-token company)
    (when token
      <div class= "alert alert-warning small mx-3">
        You're using a recovery token. It only gives access to the SSO and Team pages, until
        ,(local-time:format-rfc1123-timestring
          nil
          (local-time:universal-to-timestamp deadline)).
      </div>)))

(defmethod render-menu-items (installation
                              (vc recovery-viewer-context)
                              &key script-name &allow-other-keys)
  "A recovery token only gives access to the Team and SSO pages, so
that's all the menu has."
  <markup:merge-tag>
    <ul class="nav nav-pills flex-column mb-auto ps-3 pe-3">
      <left-nav-item href= "/team" image-class= "group"
                     script-name=script-name >
        Team
      </left-nav-item>

      <left-nav-item href= (sso-page-url (viewer-context-company vc))
                     image-class= "lock"
                     script-name=script-name >
        SSO
      </left-nav-item>
    </ul>

    <recovery-token-banner company= (viewer-context-company vc) />

    <hr class= "mb-0" />
  </markup:merge-tag>)

(defun settings-sso-page-url ()
  "The SSO page in Settings, for logged in admins of the current company."
  "/settings/sso")

(defun sso-return-url (company)
  "Where to go back to after an action on COMPANY's SSO page: the
recovery page for a recovery token, otherwise Settings."
  (cond
    ((typep (sso-principal company) 'recovery-token)
     (sso-page-url company))
    (t
     (settings-sso-page-url))))

(defhandler (nil :uri "/sso") ()
  (with-login ()
    (hex:safe-redirect (settings-sso-page-url))))

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
          Paste the token you got from Screenshotbot support. It only gives access to your SSO settings and your team's roles.
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

(defun sso-page (company)
  <app-template>
    <sso-providers company=company />
  </app-template>)

(defun sso-settings-page ()
  (let ((company (auth:current-company)))
    (check-enabled! company)
    <settings-template>
      ,(cond
         ((sso-principal company)
          <sso-providers company=company />)
         (t
          <markup:merge-tag>
            <h3>Single sign-on</h3>
            <div class= "alert alert-info mt-3">
              Only admins can configure single sign-on. Please ask an admin of your organization if you need to change it.
            </div>
          </markup:merge-tag>))
    </settings-template>))

(defsettings sso
  :name "sso"
  :title "Single sign-on"
  :section :organization
  :handler 'sso-settings-page)

(defmethod should-show-settings-p (installation (name (eql 'sso)))
  "Everyone sees it, non-admins get a banner instead of the settings."
  (let ((company (auth:current-company)))
    (and company
         (gk:check :self-service-saml company))))

(markup:deftag sso-providers (&key company)
  <markup:merge-tag>
    <taskie-page-title title= "Configure Single-Sign-on">
      <a href= (nibble () (sso-form company)) class= "btn btn-primary mb-2" >Create SAML</a>
      <a href= (nibble () (oidc-form company)) class= "btn btn-primary mb-2" >Create OIDC</a>
    </taskie-page-title>

    <table class= "table border mt-3">
      <thead>
        <th>Type</th>
        <th>IdP Entity ID or Issuer</th>
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
                <td class= "text-nowrap" >
                  <test-button href= (nibble () (start-test-login saml)) />
                  <actions-menu>
                    <activation-button saml=saml />
                    <action-item icon= "edit" label= "Edit"
                                 href= (nibble () (edit-sso-form saml)) />
                    <li><hr class= "dropdown-divider" /></li>
                    <delete-button saml=saml />
                  </actions-menu>
                </td>
              </tr>))
      ,@(loop for oidc in (fset:convert 'list (oidc-providers-for-company company))
              collect
              (util:copying (oidc)
              <tr class= "align-middle" >
                <td>OIDC</td>
                <td><tt>,(issuer oidc)</tt></td>
                <td><oidc-test-result result= (last-oidc-test-result oidc) /></td>
                <td><oidc-activation-status oidc=oidc /></td>
                <td class= "text-nowrap" >
                  <test-button href= (nibble () (start-oidc-test-login oidc)) />
                  <actions-menu>
                    <oidc-activation-button oidc=oidc />
                    <action-item icon= "edit" label= "Edit"
                                 href= (unless (activep oidc)
                                         (nibble () (edit-oidc-form oidc)))
                                 reason= "The default connection can't be changed" />
                    <li><hr class= "dropdown-divider" /></li>
                    <oidc-delete-button oidc=oidc />
                  </actions-menu>
                </td>
              </tr>))
    </table>
  </markup:merge-tag>)

(markup:deftag test-button (&key href)
  <a href=href class= "btn btn-sm btn-primary" >
    <mdi name= "play_arrow" class= "align-middle" /> Test
  </a>)

(markup:deftag actions-menu (children)
  <div class= "dropdown d-inline-block" >
    <button class= "btn btn-sm btn-outline-secondary" type= "button"
            data-bs-toggle= "dropdown" aria-expanded= "false" aria-label= "More actions" >
      <mdi name= "more_vert" class= "align-middle" />
    </button>
    <ul class= "dropdown-menu dropdown-menu-end" >
      ,@children
    </ul>
  </div>)

(markup:deftag action-item (&key icon label href reason danger)
  "An item in ACTIONS-MENU. Without HREF it's disabled, and REASON says
why: disabled elements don't get hover events, so a tooltip wouldn't
show."
  (let ((class (format nil "dropdown-item~@[ text-danger~]" danger)))
    <li>
      ,(if href
           <a class=class href=href >
             <mdi name=icon class= "align-middle me-2" />,(progn label)
           </a>
           <span class= (format nil "~a disabled" class) aria-disabled= "true" >
             <mdi name=icon class= "align-middle me-2" />,(progn label)
             ,(when reason
                <div class= "small text-wrap" style= "max-width: 16rem" >,(progn reason)</div>)
           </span>)
    </li>))

(markup:deftag test-result (&key result)
  (cond
    ((null result)
     <span class= "text-muted">Never tested</span>)
    ((saml-test-result-error result)
     <span class= "text-danger">Failed: ,(saml-test-result-error result)</span>)
    (t
     <span class= "text-success">Signed in as ,(saml-test-result-email result)</span>)))

(markup:deftag oidc-test-result (&key result)
  (cond
    ((null result)
     <span class= "text-muted">Never tested</span>)
    ((oidc-test-result-error result)
     <span class= "text-danger">Failed: ,(oidc-test-result-error result)</span>)
    (t
     <span class= "text-success">Signed in as ,(oidc-test-result-email result)</span>)))

(markup:deftag activation-status (&key saml)
  (cond
    ((defaultp saml)
     <span class= "badge bg-success">Default</span>)
    (t
     <markup:merge-tag>
       <div class= "text-muted">
         ,(or (activation-blocker saml (sso-principal (saml-company saml)))
              "Ready to set as default")
       </div>
       <div class= "small text-muted">
         Not the default, but users can still sign in with it from your IdP or from <a href= (login-all-url) >all sign-in options</a>.
       </div>
     </markup:merge-tag>)))

(markup:deftag oidc-activation-status (&key oidc)
  (cond
    ((activep oidc)
     <span class= "badge bg-success">Default</span>)
    (t
     <markup:merge-tag>
       <div class= "text-muted">
         ,(or (oidc-activation-blocker oidc (sso-principal (oidc-provider-company oidc)))
              "Ready to set as default")
       </div>
       <div class= "small text-muted">
         Not the default, but users can still sign in with it from <a href= (login-all-url) >all sign-in options</a>.
       </div>
     </markup:merge-tag>)))

(markup:deftag oidc-activation-button (&key oidc)
  ;; There's no Unset Default for OIDC, see the docs.
  (unless (activep oidc)
    (let ((blocker (oidc-activation-blocker oidc (sso-principal (oidc-provider-company oidc)))))
      <action-item icon= "star" label= "Set Default"
                   href= (unless blocker
                           (nibble () (%activate-oidc oidc)))
                   reason=blocker />)))

(markup:deftag activation-button (&key saml)
  (cond
    ((defaultp saml)
     <action-item icon= "star_border" label= "Unset Default"
                  href= (nibble () (confirm-deactivate saml)) />)
    (t
     (let ((blocker (activation-blocker saml (sso-principal (saml-company saml)))))
       <action-item icon= "star" label= "Set Default"
                    href= (unless blocker
                            (nibble () (%activate saml)))
                    reason=blocker />))))

(markup:deftag delete-button (&key saml)
  <action-item icon= "delete" label= "Delete" danger=t
               href= (unless (defaultp saml)
                       (let ((company (saml-company saml)))
                         (nibble () (confirm-delete saml company))))
               reason= "Unset this connection as the default before deleting it" />)

(markup:deftag oidc-delete-button (&key oidc)
  <action-item icon= "delete" label= "Delete" danger=t
               href= (unless (activep oidc)
                       (let ((company (oidc-provider-company oidc)))
                         (nibble () (confirm-delete-oidc oidc company))))
               reason= "Set another connection as the default before deleting this one" />)

(defun confirm-delete-oidc (oidc company)
  (check-principal! company)
  (confirmation-page
   :yes (nibble () (%delete-oidc oidc company))
   :no (sso-return-url company)
   :danger t
   <p>Delete the OIDC connection for <b>,(issuer oidc)</b>? Its test results will be deleted too.</p>))

(defun %delete-oidc (oidc company)
  "COMPANY is captured when the page renders, since OIDC might already be
deleted (e.g. a double click)."
  (check-principal! company)
  (unless (bknr.indices:object-destroyed-p oidc)
    (assert (eql company (oidc-provider-company oidc)))
    (delete-default-oidc-provider oidc))
  (go-back company))

(defun confirm-delete (saml company)
  (check-principal! company)
  (confirmation-page
   :yes (nibble () (%delete saml company))
   :no (sso-return-url company)
   :danger t
   <p>Delete the SAML connection <b>,(saml-name saml)</b>? Its test results will be deleted too.</p>))

(defun confirm-deactivate (saml)
  (check-provider! saml)
  (confirmation-page
   :yes (nibble () (%deactivate saml))
   :no (sso-return-url (saml-company saml))
   :danger t
   <p>Stop using the SAML connection <b>,(saml-name saml)</b> as the default? The sign-in page will stop sending users to it. It will still accept sign-ins that start from your IdP, so delete it to turn it off completely.</p>))

(defun %delete (saml company)
  "COMPANY is captured when the page renders, since SAML might already be
deleted (e.g. a double click)."
  (check-principal! company)
  (unless (bknr.indices:object-destroyed-p saml)
    (assert (eql company (saml-company saml)))
    (delete-saml-provider saml))
  (go-back company))

(defun check-provider! (saml)
  "Nibbles re-check access, since admin rights could have changed (or the
recovery token expired) since the page was rendered. Returns the
principal."
  (check-principal! (saml-company saml)))

(defun go-back (company)
  (hex:safe-redirect (sso-return-url company)))

(defun %activate (saml)
  (activate-saml-provider saml (check-provider! saml))
  ;; The active OIDC provider would take precedence over SAML, see
  ;; DEFAULT-OIDC-PROVIDER on BASE-INSTALLATION.
  (alexandria:when-let ((oidc (active-default-oidc-provider)))
    (deactivate-default-oidc-provider oidc))
  (go-back (saml-company saml)))

(defun %activate-oidc (oidc)
  (let ((company (oidc-provider-company oidc)))
    (activate-default-oidc-provider oidc (check-principal! company))
    ;; Only one SSO provider should be active, otherwise it's unclear
    ;; which one is used to sign in.
    (alexandria:when-let ((saml (default-saml-auth-provider)))
      (deactivate-saml-provider saml))
    (go-back company)))

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
                                       (sso-return-url (saml-company saml)))))

(defun start-oidc-test-login (oidc)
  (let ((company (oidc-provider-company oidc)))
    (hex:safe-redirect (oidc-test-signin-link oidc
                                              (check-principal! company)
                                              (sso-return-url company)))))

(markup:deftag copyable-value (&key label value)
  ;; No NAME, so this isn't submitted with the form.
  <div class= "mb-2">
    <label class= "form-label">,(progn label)</label>
    <div class= "input-group">
      <input type= "text" class= "form-control font-monospace" readonly= "readonly" value=value />
      <button type= "button" class= "btn btn-outline-secondary"
              onclick= "navigator.clipboard.writeText(this.parentElement.querySelector('input').value)" >Copy</button>
    </div>
  </div>)

(defun sp-metadata-url (company)
  "The URL IdPs can import our metadata from."
  (cond
    ((typep *installation* 'multi-org-feature)
     (hex:make-full-url hunchentoot:*request* "/saml/entity/:oid/metadata" :oid (oid company)))
    (t
     ;; Same as the entity ID, see SINGLETON-SP-METADATA
     (entity-id))))

(markup:deftag sp-settings (&key company)
  "What the customer configures in their IdP. These come from the same
functions the login flow uses, so they can't drift apart."
  <div class= "mb-3">
    <h5>Settings for your Identity Provider</h5>
    <div class= "text-muted mb-2">
      Configure your IdP with these. If your IdP can import SAML metadata from a URL, you can use the metadata URL instead.
    </div>
    <copyable-value label= "Single sign-on URL (ACS URL, also the Recipient and Destination)"
                    value= (callback-url) />
    <copyable-value label= "Audience URI (SP Entity ID)"
                    value= (entity-id) />
    <copyable-value label= "SP metadata URL"
                    value= (sp-metadata-url company) />
    <ul class= "text-muted">
      <li>Name ID format: EmailAddress</li>
      <li>Application username: Email</li>
      <li>Response and assertion: signed, RSA-SHA256</li>
    </ul>
  </div>)

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
      
      <sp-settings company=company />

      <div class= "mb-2" >
        <label for= "idp-metadata" class= "form-label">Identity Provider Metadata URL</label>
        ,(when url-locked-p
           <div class= "text-muted">
             This is the default connection, so its IdP can't be changed. To switch IdPs, create a new connection, test it and set it as the default.
           </div>)
        <input type= "text" name= "idp-metadata" id= "idp-metadata" class= "form-control"   placeholder= "https://idp.example.com/saml/metadata"
               value= (when saml (saml-idp-metadata-url saml))
               readonly= (when url-locked-p "readonly") />
      </div>
      
      <div class= "card-footer">
        <p class= "text-muted">
          ,(if saml
               "If you change the IdP, you'll need to test the connection again before setting it as the default"
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
           "This is the default connection, so its IdP can't be changed")
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

(defparameter *oidc-scope* "openid email profile")

(defun oidc-redirect-uri ()
  "The redirect URI the IdP must allow. This is what MAKE-OAUTH-URL uses,
since the providers we create don't have an RP-BASE-URL."
  (hex:make-full-url hunchentoot:*request* "/account/oauth-callback"))

(defun edit-oidc-form (oidc)
  (oidc-form (oidc-provider-company oidc) :oidc oidc))

(defun oidc-form (company &key oidc)
  "The form to create an OIDC provider, or to edit OIDC if it's given."
  (check-principal! company)
  (let ((action (nibble (issuer client-id client-secret)
                  (submit-oidc-form company
                                    :oidc oidc
                                    :issuer issuer
                                    :client-id client-id
                                    :client-secret client-secret))))
    <simple-card-page form-action=action >
      <div class= "card-header">
        <h4 class= "card-title">
          ,(if oidc "Edit OIDC connection" "New OIDC connection")
        </h4>
      </div>

      <div class= "mb-3">
        <h5>Settings for your Identity Provider</h5>
        <copyable-value label= "Sign-in redirect URI" value= (oidc-redirect-uri) />
        <ul class= "text-muted">
          <li>Grant type: Authorization Code</li>
          <li>Scopes: ,(progn *oidc-scope*)</li>
        </ul>
      </div>

      <div class= "mb-2" >
        <label for= "issuer" class= "form-label">Issuer URL</label>
        <input type= "text" id= "issuer" name= "issuer" class= "form-control"
               placeholder= "https://idp.example.com"
               value= (when oidc (issuer oidc)) />
      </div>

      <div class= "mb-2" >
        <label for= "client-id" class= "form-label">Client ID</label>
        <input type= "text" id= "client-id" name= "client-id" class= "form-control"
               value= (when oidc (client-id oidc)) />
      </div>

      <div class= "mb-2" >
        <label for= "client-secret" class= "form-label">Client Secret</label>
        ,(when oidc
           <div class= "text-muted">
             Leave this blank to keep the current secret
           </div>)
        <input type= "password" id= "client-secret" name= "client-secret" class= "form-control" />
      </div>

      <div class= "card-footer">
        <p class= "text-muted">
          ,(if oidc
               "If you change anything, you'll need to test the connection again before setting it as the default"
               "Until it's set as the default, users can only sign in with it from all sign-in options")
        </p>
        <input type= "submit" class= "btn btn-primary" value= (if oidc "Save" "Create") />
      </div>
    </simple-card-page>))

(defun submit-oidc-form (company &key oidc issuer client-id client-secret)
  (check-principal! company)
  (when oidc
    (assert (eql company (oidc-provider-company oidc))))
  (with-error-builder (:errors errors
                       :check check
                       :form-builder (oidc-form company :oidc oidc)
                       ;; Don't send the secret back to the browser
                       :form-args (:issuer issuer :client-id client-id)
                       :success (cond
                                  (oidc
                                   (edit-default-oidc-provider
                                    oidc
                                    :issuer issuer
                                    :client-id client-id
                                    :client-secret (when (str:non-blank-string-p client-secret)
                                                     client-secret))
                                   (go-back company))
                                  (t
                                   (%create-oidc company
                                                 :issuer issuer
                                                 :client-id client-id
                                                 :client-secret client-secret))))
    (check :issuer
           (not (and oidc (activep oidc)))
           "This is the default connection, so it can't be changed")
    (check :issuer
           (str:non-blank-string-p issuer)
           "Issuer URL must be provided")
    (check :issuer
           (equal "https" (ignore-errors (quri:uri-scheme (quri:uri issuer))))
           "Issuer URL must be an https:// URL")
    (check :client-id
           (str:non-blank-string-p client-id)
           "Client ID must be provided")
    (check :client-secret
           (or oidc
               (str:non-blank-string-p client-secret))
           "Client Secret must be provided")
    (when (null errors)
      (check :issuer
             (ignore-errors
              (discover (make-instance 'oidc :issuer issuer)))
             "Could not fetch the OpenID configuration for this issuer, if this is behind an internal network please contact support@screenshotbot.io to help set it up"))))

(defun %create-oidc (company &key issuer client-id client-secret)
  (make-instance 'default-oidc-provider
                 :company company
                 :issuer issuer
                 :client-id client-id
                 :client-secret client-secret
                 :scope *oidc-scope*)
  (go-back company))

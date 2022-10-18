(defpackage :screenshotbot/sso/settings
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:defsettings
                #:settings-template)
  (:import-from #:screenshotbot/template
                #:mailto)
  (:import-from #:nibble
                #:defnibble
                #:nibble)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/sso/settings)

(markup:enable-reader)

(markup:deftag linput (&key label name value (type "text") placeholder
                       disabled)
  <div class= "mb-3" >
    <label for=name class= "form-label" >,(progn label)</label>
    <input class= "form-control" type=type name=name id=name value=value placeholder=placeholder
           disabled=disabled />
  </div>)

(defnibble submit-sso (issuer client-id client-secret scope)
  (with-form-errors (:errors `("You must be admin to update SSO settings")
                     :was-validated t
                     :issuer issuer
                     :client-id client-id
                     :client-secret client-secret
                     :scope scope)
    (sso-settings)))

(defun sso-settings ()
  <settings-template>

    <form action= (nibble submit-sso) >
      <div class= "card mt-3">
        <div class= "card-header">
          <h3 class= "" >Single-Sign-On</h3>
          <p>SSO is included in every plan at no extra cost. Please contact <mailto>support@screenshotbot.io</mailto> if you need to use SAML authentication instead of OpenID Connect.</p>

        </div>
        <div class= "card-body">
          <div class= "alert alert-danger d-none" />
          <linput label= "OpenID Connect Issuer" name="issuer" placeholder= "https://auth.example.com/auth" />
          <linput label= "Client ID" name="client-id" />
          <linput label= "Client Secret" name="client-secret" />
          <linput label= "Scope" name="client-secret" value= "openid email profile"
                  disabled= "disabled" />


        </div>

        <div class= "card-footer">
          <input type= "submit" class= "btn btn-primary" value= "Save" />
          <input type= "button" formaction=nil
                 class= "btn btn-secondary" value= "Test" />
        </div>
      </div>
    </form>


  </settings-template>)

(defsettings sso
  :name "sso"
  :title "Single sign-on"
  :staging-p t
  :section nil
  :handler 'sso-settings)

(defpackage :screenshotbot/sso/settings
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:defsettings
                #:settings-template)
  (:import-from #:screenshotbot/template
                #:mailto)
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

(defun sso-settings ()
  <settings-template>

    <div class= "card mt-3">
      <div class= "card-header">
        <h3 class= "" >Single-Sign-On</h3>
        #-screenshotbot-oss
        <p>SSO is included in every plan at no extra cost. Please contact <mailto>support@screenshotbot.io</mailto> if you need to use SAML authentication instead of OpenID Connect.</p>

      </div>
      <div class= "card-body">
        <form>
          <linput label= "OpenID Connect Issuer" name="client-secret" placeholder= "https://auth.example.com/auth" />
          <linput label= "Client ID" name="client-id" />
          <linput label= "Client Secret" name="client-secret" />
          <linput label= "Scope" name="client-secret" value= "openid email profile"
                  disabled= "disabled" />

        </form>

      </div>

      <div class= "card-footer">
        <input type= "submit" class= "btn btn-primary" value= "Save" />
        <input type= "button" formaction=nil
               class= "btn btn-secondary" value= "Test" />
      </div>
    </div>

  </settings-template>)

(defsettings sso
  :name "sso"
  :title "Single sign-on"
  :staging-p t
  :section nil
  :handler 'sso-settings)

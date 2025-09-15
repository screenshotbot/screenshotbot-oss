(defsystem :auth.login
  :serial t
  :depends-on (:auth
               :auth.model
               :util.statsig
               :hunchentoot-extensions
               :nibble
               :core.installation
               :util
               :util/throttler
               :util/events
               :util/recaptcha
               :util/request
               :util.store
               :clavier
               :oidc)
  :components ((:file "impersonation")
               (:file "roles-auth-provider")
               (:file "cached-avatar")
               (:file "common")
               (:file "github")
               (:file "oidc")
               (:file "login")
               (:file "sso")
               (:file "signup")
               (:file "verify-email")
               (:file "saml")
               (:file "forgot-password")))

(defsystem :auth.login/tests
  :serial t
  :depends-on (:auth.login
               :fiveam-matchers
               :util/fiveam)
  :components ((:file "test-roles-auth-provider")
               (:file "test-verify-email")
               (:file "test-cached-avatar")
               (:file "test-sso")))

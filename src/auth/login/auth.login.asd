(defsystem :auth.login
    :serial t
    :depends-on (:auth
                 :auth.model
                 :hunchentoot-extensions
                 :nibble
                 :core.installation
                 :util
                 :util/throttler
                 :util/events
                 :util.store
                 :clavier
                 :oidc)
    :components ((:file "impersonation")
                 (:file "common")
                 (:file "github")
                 (:file "oidc")
                 (:file "login")
                 (:file "signup")
                 (:file "forgot-password")))

(defsystem :auth.login/tests
    :serial t
    :depends-on (:auth.login
                 :util/fiveam))

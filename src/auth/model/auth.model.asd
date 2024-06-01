(defsystem :auth.model
  :serial t
  :depends-on (:auth
               :util
               :util.store)
  :components ((:file "user")
               (:file "roles")
               (:file "company-sso")
               (:file "invite")
               (:file "email-confirmation")))

(defsystem :auth.model/tests
  :serial t
  :depends-on (:auth.model
               :fiveam-matchers
               :util/fiveam)
  :components ((:file "test-email-confirmation")
               (:file "test-invite")
               (:file "test-roles")))

(defsystem :auth.model
  :serial t
  :depends-on (:auth
               :util
               :util.store)
  :components ((:file "user")
               (:file "email-confirmation")))

(defsystem :auth.model/tests
  :serial t
  :depends-on (:auth.model
               :util/fiveam)
  :components ((:file "test-email-confirmation")))

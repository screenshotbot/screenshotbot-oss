(defsystem "auth.tests"
  :serial t
  :depends-on (:auth
               :fiveam)
  :components ((:file "test-auth")))

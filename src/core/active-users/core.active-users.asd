(defsystem :core.active-users
  :serial t
  :depends-on (:util.store
               :local-time)
  :components ((:file "active-users")))

(defsystem :core.active-users/tests
  :serial t
  :depends-on (:core.active-users
               :fiveam-matchers
               :util/fiveam)
  :components ((:file "test-active-users")))

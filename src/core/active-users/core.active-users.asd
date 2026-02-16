(defsystem :core.active-users
  :serial t
  :depends-on (:util.store
               :util/atomics
               :auth.login
               :hunchentoot
               :util/cron
               :local-time)
  :components ((:file "active-users")))

(defsystem :core.active-users/tests
  :serial t
  :depends-on (:core.active-users
               :fiveam-matchers
               :util.testing
               :util/fiveam)
  :components ((:file "test-active-users")))

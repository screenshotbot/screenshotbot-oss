(defsystem :core.cli
  :serial t
  :depends-on (:util/threading
               :easy-macros)
  :components ((:file "sentry")))

(defsystem :core.cli/tests
  :serial t
  :depends-on (:core.cli
               :util/fiveam)
  :components ((:file "test-sentry")))

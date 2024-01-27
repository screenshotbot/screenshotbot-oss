(defsystem :core.cli
  :serial t
  :depends-on (:util/threading
               :easy-macros)
  :components ((:file "sentry")))

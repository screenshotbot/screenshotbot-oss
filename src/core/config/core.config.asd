(defsystem :core.config
  :serial t
  :depends-on (:bknr.datastore
               :util.misc
               :util.store)
  :components ((:file "model")))

(defsystem :core.config/tests
  :serial t
  :depends-on (:core.config
               :util/fiveam)
  :components ((:file "test-model")))

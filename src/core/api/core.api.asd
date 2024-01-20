(defsystem :core.api
  :serial t
  :components ((:file "api-key-api")
               (:module "model"
                :components ((:file "api-key")))))

(defsystem :core.api/tests
  :serial t
  :depends-on (:core.api
               :util/fiveam
               :fiveam-matchers)
  :components ((:module "model"
                :components ((:file "test-api-key")))))

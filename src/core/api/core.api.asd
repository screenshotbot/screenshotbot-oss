(defsystem :core.api
  :serial t
  :components ((:file "api-key-api")
               (:module "model"
                :components ((:file "api-key")))))

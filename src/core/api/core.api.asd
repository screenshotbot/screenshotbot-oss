(defsystem :core.api
  :serial t
  :depends-on (:markup
               :auth.login
               :parenscript
               :core.ui
               :util/timeago
               :util.store)
  :components ((:file "acceptor")
               (:file "api-key-api")
               (:module "model"
                :components ((:file "api-key")))
               (:module "dashboard"
                :components ((:file "api-keys")))))

;; NOTE: dashboard/test-api-keys is still in screenshotbot
;; TODO(T1005, T1006)
(defsystem :core.api/tests
  :serial t
  :depends-on (:core.api
               :util/fiveam
               :fiveam-matchers)
  :components ((:module "model"
                :components ((:file "test-api-key")))))

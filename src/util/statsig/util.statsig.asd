(defsystem :util.statsig
  :depends-on (:util/request
               :yason
               :core.installation
               :auth
               :util/atomics
               :util/cron
               :atomics
               :log4cl
               :alexandria)
  :serial t
  :components ((:file "statsig")))

(defsystem :util.statsig/tests
  :depends-on (:util.statsig
               :fiveam-matchers
               :util.testing
               :util/fiveam)
  :components ((:file "test-statsig")))

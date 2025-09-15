(defsystem :util.statsig
  :depends-on (:util/request
               :yason
               :core.installation
               :auth
               :atomics
               :log4cl
               :alexandria)
  :serial t
  :components ((:file "statsig")))

(defsystem :util.statsig/tests
  :depends-on (:util.statsig
               :util/fiveam)
  :components ((:file "test-statsig")))

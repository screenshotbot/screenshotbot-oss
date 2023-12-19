(defsystem :test-runner
  :serial t
  ;; Most of these dependencies are because we need these to be loaded
  ;; before we can load any test deps with
  ;; dspect:*redefinition-action*.
  :depends-on (#-(or jipr eaase-oss)
               :jvm
               :trivial-features
               :log4cl
               :fiveam
               #-(:or :jipr :screenshotbot-oss :eaase-oss)
               :sentry
               :graphs
               :util/testing
               :str
               :util/misc
               :util/threading
               :tmpdir
               :cl-fad)
  :components ((:file "affected-systems")
               (:file "test-runner")))

(defsystem :test-runner
  :serial t
  ;; Most of these dependencies are because we need these to be loaded
  ;; before we can load any test deps with
  ;; dspect:*redefinition-action*.
  :depends-on (#-jipr
               :jvm
               :trivial-features
               :fiveam
               #-screenshotbot-oss
               :sentry
               :testing)
  :components ((:file "test-runner")))

(defsystem pixel-diff.math
  :depends-on (:screenshotbot.js-assets/package)
  :serial t
  :components ((:file "lisp-stubs")
               (:file "common-ps")))

(defsystem screenshotbot.math/tests
  :serial t
  :depends-on (:screenshotbot.js-assets/lisp
               :util/fiveam)
  :components ((:file "test-common-ps")))



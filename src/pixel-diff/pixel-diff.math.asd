(defsystem pixel-diff.math/package
  :depends-on (:parenscript
               :3d-matrices)
  :components ((:file "package")))

(defsystem pixel-diff.math
  :depends-on (:pixel-diff.math/package)
  :serial t
  :components ((:file "lisp-stubs")
               (:file "common-ps")))

(defsystem pixel-diff.math/tests
  :serial t
  :depends-on (:pixel-diff.math
               :util/fiveam)
  :components ((:file "test-common-ps")))



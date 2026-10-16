(defsystem pixel-diff.math-js
  :class "build-utils:js-system"
  :serial t
  :defsystem-depends-on (:build-utils)
  :depends-on (:pixel-diff.math/package)
  :components (("build-utils:ps-file" "common-ps")))

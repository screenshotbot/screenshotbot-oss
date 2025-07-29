(defsystem :pixel-diff
  :serial t
  :depends-on (:pixel-diff.math
               :util/misc)
  :components ((:file "differ")
               (:file "main")))

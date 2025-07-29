(defsystem :pixel-diff
  :serial t
  :depends-on (:pixel-diff.math)
  :components ((:file "differ")
               (:file "main")))

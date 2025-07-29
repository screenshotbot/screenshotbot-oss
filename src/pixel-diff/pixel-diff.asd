(defsystem :pixel-diff
  :serial t
  :depends-on (:pixel-diff.math)
  :components ((:file "about")
               (:file "differ")
               (:file "main")))

(defsystem :pixel-diff
  :serial t
  :depends-on (:pixel-diff.math
               :easy-macros)
  :components ((:file "about")
               (:file "differ")
               (:file "main")))

(defsystem :pixel-diff
  :serial t
  :depends-on (:pixel-diff.math
               :easy-macros)
  :components ((:file "about")
               #+darwin
               (:file "fli-templates")
               (:file "differ")
               (:file "main")))

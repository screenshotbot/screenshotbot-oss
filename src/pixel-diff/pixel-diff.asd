(defsystem :pixel-diff
  :serial t
  :depends-on (:pixel-diff.math
               :easy-macros
               :log4cl)
  :components ((:file "about")
               #+darwin
               (:file "fli-templates")
               (:file "differ")
               (:file "main")))

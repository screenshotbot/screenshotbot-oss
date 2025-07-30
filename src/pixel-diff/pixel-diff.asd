(defsystem :pixel-diff
  :serial t
  :depends-on (:pixel-diff.math
               :easy-macros)
  :components ((:file "about")
	       (:file "fli-templates")
               (:file "differ")
               (:file "main")))

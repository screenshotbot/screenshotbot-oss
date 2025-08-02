(defsystem :pixel-diff
  :serial t
  :depends-on (:pixel-diff.math
               :easy-macros
               :clingon
               :log4cl)
  :components ((:file "about")
               #+darwin
               (:file "fli-templates")
               (:file "differ")
               (:file "main")))

#+lispworks
(defsystem :pixel-diff/tests
  :serial t
  :depends-on (:pixel-diff
               :fiveam))

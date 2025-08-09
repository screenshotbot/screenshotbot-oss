(defsystem :pixel-diff
  :serial t
  :depends-on (:pixel-diff.math
               :easy-macros
               :clingon
               :cl-fad
               :log4cl)
  :components ((:file "about")
               (:file "image-pair")
               #+darwin
               (:file "fli-templates")
               (:file "git-diff")
               (:file "differ")
               (:file "browser")
               (:file "main")))

#+lispworks
(defsystem :pixel-diff/tests
  :serial t
  :depends-on (:pixel-diff
               :fiveam)
  :components ((:file "suite")
               (:file "test-differ")))

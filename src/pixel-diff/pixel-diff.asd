(defsystem :pixel-diff
  :version "1.0.0"
  :serial t
  :depends-on (:pixel-diff.math
               :easy-macros
               :clingon
               :util.threading
               :cl-fad
               :log4cl)
  :components ((:file "about")
               (:file "usage")
               (:file "image-pair")
               #+darwin
               (:file "fli-templates")
               (:file "differ")
               (:file "browser")
               (:file "git-diff")
               (:file "main")))

#+lispworks
(defsystem :pixel-diff/tests
  :serial t
  :depends-on (:pixel-diff
               :fiveam)
  :components ((:file "suite")
               (:file "test-differ")))

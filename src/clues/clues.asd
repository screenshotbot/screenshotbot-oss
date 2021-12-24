(defsystem :clues
  :serial t
  :depends-on (:alexandria
               :closer-mop)
  :components ((:file "injector")
               (:file "module")
               (:file "injectable")
               (:file "all")))

(defsystem :clues/tests
  :serial t
  :depends-on (:clues
               :fiveam-matchers
               :util/fiveam)
  :components ((:file "test-injector")
               (:file "test-module")
               (:file "test-injectable")))

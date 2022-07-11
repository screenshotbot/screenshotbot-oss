(defsystem :testing
  :serial t
  :depends-on (:graphs
               :cl-fad)
  :components ((:file "affected-systems")))

(defsystem :testing/tests
  :serial t
  :depends-on (:testing))

(defsystem :fiveam-matchers
  :serial t
  :depends-on (:fiveam
               :pkg)
  :components ((:file "core")
               (:file "lists")
               (:file "has-length")
               (:file "every-item")
               (:file "strings")
               (:file "all")))

(defsystem :fiveam-matchers/tests
  :serial t
  :depends-on (:fiveam-matchers)
  :components ((:file "test-core")
               (:file "test-lists")))

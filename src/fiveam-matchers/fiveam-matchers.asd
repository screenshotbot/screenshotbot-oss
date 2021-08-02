(defsystem :fiveam-matchers
  :serial t
  :depends-on (:fiveam
               :pkg)
  :components ((:file "core")
               (:file "lists")
               (:file "all")))

(defsystem :fiveam-matchers/tests
  :serial t
  :depends-on (:fiveam-matchers)
  :components ((:file "test-core")
               (:file "test-lists")))

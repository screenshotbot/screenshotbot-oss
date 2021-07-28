(defsystem :deadbeef
  :serial t
  :depends-on ()
  :components ((:file "impl")
               (:file "all")))

(defsystem :deadbeef/tests
  :serial t
  :depends-on (:deadbeef
               :pkg
               :fiveam)
  :components ((:file "test-impl")))

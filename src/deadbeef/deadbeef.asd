(defsystem :deadbeef
  :serial t
  :depends-on (:cl-fad
               :pkg)
  :components ((:file "impl")
               (:file "all")))

(defsystem :deadbeef/tests
  :serial t
  :depends-on (:deadbeef
               :fiveam)
  :components ((:file "test-impl")))

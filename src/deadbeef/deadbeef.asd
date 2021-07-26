(defsystem :deadbeef
  :serial t
  :components ((:file "impl")))

(defsystem :deadbeef/tests
  :serial t
  :depends-on (:deadbeef
               :fiveam)
  :components ((:file "test-impl")))

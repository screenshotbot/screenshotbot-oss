(defsystem :util.testing
  :serial t
  :depends-on (:fiveam
               :cl-mock
               :lparallel
               :easy-macros
               :auth)
  :components ((:file "testing")))

(defsystem :util.testing/tests
  :depends-on (:util.testing
               :fiveam)
  :components ((:file "test-testing")))

(defsystem :util.testing
  :serial t
  :depends-on (:fiveam
               :cl-mock
               :lparallel
               :nibble
               :util/random-port
               :easy-macros
               :auth)
  :components ((:file "testing")))

(defsystem :util.testing/tests
  :depends-on (:util.testing
               :util/fiveam
               :fiveam)
  :components ((:file "test-testing")))

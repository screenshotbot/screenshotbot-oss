(defsystem :graphs
  :serial t
  :depends-on (:alexandria)
  :components ((:file "dfs")
               (:file "all")))

(defsystem :graphs/tests
  :serial t
  :depends-on (:graphs
               :util/fiveam)
  :components ((:file "test-dfs")))

(defsystem :graphs
  :serial t
  :components ((:file "dfs")
               (:file "all")))

(defsystem :graphs/tests
  :serial t
  :depends-on (:graphs)
  :components ((:file "test-dfs")))

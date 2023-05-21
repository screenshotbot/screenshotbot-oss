(defsystem :zookeeper
    :serial t
    :depends-on (:str
                 :easy-macros
                 :atomics)
    :components ((:file "zookeeper")))

#+(and lispworks linux)
(defsystem :zookeeper/tests
    :serial t
    :depends-on (:zookeeper
                 :fiveam))

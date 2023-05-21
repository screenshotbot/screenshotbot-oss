(defsystem :zookeeper
    :serial t
    :depends-on (:str
                 :easy-macros
                 :atomics)
    :components ((:file "zookeeper")))

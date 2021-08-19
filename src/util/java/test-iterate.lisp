(pkg:define-package :util/java/test-iterate
    (:use #:cl
          #:fiveam
          #:iterate
          #:alexandria)
  (:import-from #:util/java/java
                #:new-instance))


(util/fiveam:def-suite)

(named-readtables:in-readtable util/java:java-syntax)

(test simple-iterate
  (let ((list (new-instance #,java.util.ArrayList)))
    (#_add list "one")
    (#_add list "two")
    (#_add list "three")
    (is (equal (list "one" "two" "three")
               (iter (for x in-java list)
                 (collect x))))))

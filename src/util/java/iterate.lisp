(pkg:define-package :util/java/iterate
    (:use #:cl
          #:iterate
          #:alexandria)
  (:import-from #:util/java/reader
                #:java-syntax)
  (:import-from #:util/java/java
                #:define-java-callers
                #:java-equals
                #:*btrue*)
  (:export
   #:in-java))

(named-readtables:in-readtable java-syntax)

(defun p (x)
  (log:info "Got: ~S" x)
  x)

(define-java-callers "java.util.Iterator"
  (has-next-p "hasNext")
  (jnext "next"))

(defmacro-driver (for var in-java x)
  (alexandria:with-gensyms (xv)
    (let ((kwd (if generate 'generate 'for)))
      `(progn
         (with ,xv = (#_iterator ,x))
         (,kwd ,var next (if (has-next-p ,xv)
                             (jnext ,xv)
                             (terminate)))))))

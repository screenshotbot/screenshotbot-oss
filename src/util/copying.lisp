(uiop:define-package :util/copying
  (:use #:cl
        #:alexandria)
  (:export
   #:copying))
(in-package :util/copying)


(defmacro copying ((&rest bindings) &body body)
  "Creates a closure copy of each of the bindings. LOOP/ITERATE/DOLIST
  don't create a new local copy of the bindings, so this is useful inside loops. e.g.

  (loop for x in (list ...)
        collect
           (copying (x)
             (lambda ()
               ;; ... do things with x ...)))
"
  `(let ,(loop for var in bindings
               collect `(,var ,var))
     ,@body))

(uiop:define-package :util/ret-let
    (:use #:cl
          #:alexandria)
  (:export
   #:ret-let))
(in-package :util/ret-let)

(defmacro ret-let ((val expr) &body body)
  "Evaluates expr and stores in val. Evaluate body with val
  bound. Returns val."
  `(let ((,val ,expr))
     ,@body
     ,val))

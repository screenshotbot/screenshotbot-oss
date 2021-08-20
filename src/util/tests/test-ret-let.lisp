(pkg:define-package :util/test-ret-let
    (:use #:cl
          #:fiveam
          #:alexandria)
  (:import-from #:util/ret-let
                #:ret-let))


(util/fiveam:def-suite)

(test simple-ret-let
  (is (eql 3 (ret-let (x (+  1 2))
               "two")))
  (is (equal "arn0ld"
             (ret-let (x "arnold")
               (setf (elt x 3) #\0)))))

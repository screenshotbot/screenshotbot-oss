(defpackage :server/test-util
  (:use #:cl
        #:fiveam)
  (:import-from #:server/util
                #:with-error-handlers))
(in-package :server/test-util)

(util/fiveam:def-suite)

(def-fixture state ()
  (eval `(defun foo (x)))
  (&body))

(test simple-invocation ()
  (with-fixture state ()
    (finishes
      (with-error-handlers ()
       (eval `(defmethod foo (x) :reset))))
    (is (eql :reset (foo nil)))))

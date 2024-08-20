(defpackage :util/tests/test-benchmark
  (:use #:cl
        #:fiveam)
  (:import-from #:util/benchmark
                #:*min-benchmark-time*
                #:benchmark)
  (:import-from #:easy-macros
                #:def-easy-macro))
(in-package :util/tests/test-benchmark)

(util/fiveam:def-suite)

(benchmark:def-benchmark foobar ()
  (benchmark:measure
    (sxhash "foobar")))

(def-easy-macro make-benchmark (&fn fn)
  (make-instance 'benchmark
                 :impl (lambda ()
                         (funcall fn))))

(test simple-invocation ()
  (let ((result
          (benchmark:run
           (make-benchmark ()
             (let ((*min-benchmark-time* 1000))
               (benchmark:measure
                 (sxhash (concatenate 'string "foo" "bar"))
                 nil))))))
    (is-true result)
    (is (numberp result))))

(defun foobar ()
  (benchmark:measure
    ;;(log:info "hello ~a" (incf ctr))
    (sxhash (concatenate 'string "foo" "bar"))
    nil))

(test run-existing-benchmark ()
  (let ((*min-benchmark-time* 1000))
    (is (numberp
         (benchmark:run 'foobar)))))

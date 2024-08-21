(defpackage :util/tests/test-benchmark
  (:use #:cl
        #:fiveam)
  (:import-from #:util/benchmark
                #:format-time
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
                 :name 'foo
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


(test format-time ()
  (is (equal "5ns" (format-time 5)))
  (is (equal "5.23us" (format-time  5231)))
  (is (equal "5.23ms" (format-time  5231000)))
  (is (equal "5.23s " (format-time  5231000000))))

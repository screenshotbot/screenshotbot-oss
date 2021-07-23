(pkg:define-package :util/tests/test-fiveam
    (:use #:cl
          #:fiveam
          #:alexandria)
  (:import-from #:util/fiveam
                #:def-suite-recursive
                #:guess-suite-name)
  (:import-from #:fiveam
                #:get-test))

(def-suite* :util/tests/test-fiveam)

(test def-suite-recursive ()
  (let ((fiveam::*toplevel-suites* nil)
        (fiveam::*test* (make-hash-table :test #'eql)))
    (def-suite-recursive :foo/bar/car)
    (let ((test (get-test :foo/bar/car)))
      (is (typep test 'fiveam::test-suite))
      (is (not (null test)))
      (def-suite-recursive :foo/bar/car)

      (is (eql test (get-test :foo/bar/car))
          "Test-suite should not be recreated")

      (def-suite-recursive :foo/bar/dar)
      (is (eql test (get-test :foo/bar/car))
          "Tests stored on parent test should not change")
      (def-suite-recursive :mar)
      (is (eql test (get-test :foo/bar/car)))
      (is (not (null (get-test :foo/bar))))
      (is (null (get-test :car))))))

(test package-to-suite-name
  (let ((*package* (find-package :util/tests/test-fiveam)))
    (is (eql :util/tests/test-fiveam
             (guess-suite-name)))))

(test def-suite-sets-*suite*
  (let ((fiveam::*toplevel-suites* nil)
        (fiveam::*suite* nil)
        (fiveam::*tests* nil))
    (util/fiveam:def-suite)
    (is (eql (fiveam::get-test :util/tests/test-fiveam)
             fiveam::*suite*))
    (pass)))

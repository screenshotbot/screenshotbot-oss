(uiop:define-package :fiveam-matchers/test-lists
    (:use #:cl
          #:fiveam
          #:alexandria)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:fiveam-matchers/core
                #:matchesp
                #:equal-to
                #:assert-that))
(in-package #:fiveam-matchers/test-lists)

(def-suite* :fiveam-matchers/test-lists)

(test contains
  (let ((matcher (contains (equal-to "foobar") (equal-to "zoidberg"))))
    (assert-that (list "foobar" "zoidberg")
                 matcher)
    (is-true (matchesp matcher (list "foobar" "zoidberg")))
    (is-false (matchesp matcher (list "foobar")))
    (is-false (matchesp matcher (list "foobar" "zoidberg" "a")))
    (is-false (matchesp matcher "20"))
    #+nil
    (assert-that (list "car" "bar") (contains (equal-to "foo") (equal-to "bar")))))

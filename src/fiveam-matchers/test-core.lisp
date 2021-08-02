(pkg:define-package :fiveam-matchers/test-core
    (:use #:cl
          #:fiveam
          #:alexandria)
  (:import-from #:fiveam-matchers/core
                #:format-description
                #:assert-that
                #:is-not
                #:describe-mismatch
                #:matchesp
                #:equal-to))


(util/fiveam:def-suite)

(test preconditions
  (pass))


(defun fmt-desc (desc)
  (with-output-to-string (s)
    (format-description
     desc s)))

(test equal-to-matcher
  (let ((matcher (equal-to "foobar")))
    (is-false (matchesp matcher "zoidberg"))
    (is-true (matchesp matcher "foobar"))
    (is (equal "was \"zoidberg\""
               (fmt-desc
                (describe-mismatch matcher "zoidberg"))))))

(test is-not-matcher
  (let ((matcher (is-not (equal-to "foobar"))))
    (is-true (matchesp matcher "zoidberg"))
    (is-false (matchesp matcher "foobar"))
    (is (equal "not was \"foobar\""
               (fmt-desc
                (describe-mismatch matcher "foobar"))))))

(test assert-that
  (assert-that "foobar"
               (equal-to "foobar"))
  ;; There's probably a way to test this next line, but it's not worth
  ;; the trouble for right now
  #+nil
  (assert-that "foobar3"
               (equal-to "foobar")))

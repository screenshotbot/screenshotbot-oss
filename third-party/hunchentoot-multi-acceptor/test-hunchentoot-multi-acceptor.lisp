(defpackage :hunchentoot-multi-acceptor/tests
  (:use #:cl
        #:fiveam)
  (:import-from #:hunchentoot-multi-acceptor
                #:%compute-start2
                #:domain-matches))
(in-package :hunchentoot-multi-acceptor/tests)


(def-suite* :hunchentoot-multi-acceptor/tests)

(test domain-matches
  (is-true (domain-matches "example.com" "example.com"))
  (is-false (domain-matches "example.com" "bar.example.com"))
  (is-false (domain-matches "example.com" "car.com")))

(test domain-matches-for-wildcard
  (is-true (domain-matches "*.example.com" "bar.example.com"))
  (is-false (domain-matches "*.example.com" "example.com"))
  (is-false (domain-matches "*.example.com" "bar.car.com")))

(test compute-start2
  (is (= 3 (%compute-start2 "*.example.com" "foo.example.com")))
  (is (= 1 (%compute-start2 "*.com" "a.com")))
  (is (< (%compute-start2 "*.example.com" "short") 0))
  (is (= 1 (%compute-start2 "*.org" "x.org"))))


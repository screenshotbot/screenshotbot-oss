(defpackage :util.test-cdn
  (:use :cl
   :alexandria
        :fiveam))
(in-package :util.test-cdn)

(def-suite* :util.test-cdn)

(test should-be-set-up-by-default
  (is (numberp util.cdn:*cdn-cache-key*)))

(defpackage :util.acceptor-with-plugins
  (:use :cl
   :alexandria
        :fiveam)
  (:import-from :util
                :make-prefix-matcher))
(in-package :util.acceptor-with-plugins)

(def-suite* :util.acceptor-with-plugins)

(test check-prefix-matcher ()
  (let ((matcher (make-prefix-matcher "/foo/")))
    (is-true (cl-ppcre:scan matcher "/foo"))
    (is-false (cl-ppcre:scan matcher "/foot"))
    (is-true (cl-ppcre:scan matcher "/foo/"))
    (is-true (cl-ppcre:scan matcher "/foo/bar/car.txt"))
    (is-false (cl-ppcre:scan matcher "/blah/foo/"))
    (is-false (cl-ppcre:scan matcher "/blah/foo"))))

(defpackage :hunchentoot-extensions/test-acceptor-with-plugins
  (:use #:cl
        #:alexandria
        #:fiveam)
  (:import-from #:hex
                #:make-prefix-matcher)
  (:import-from #:hunchentoot-extensions
                #:acceptor-with-plugins)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length))
(in-package #:hunchentoot-extensions/test-acceptor-with-plugins)

(def-suite* :hunchentoot-extensions/test-acceptor-with-plugins)

(test check-prefix-matcher ()
  (let ((matcher (make-prefix-matcher "/foo/")))
    (is-true (cl-ppcre:scan matcher "/foo"))
    (is-false (cl-ppcre:scan matcher "/foot"))
    (is-true (cl-ppcre:scan matcher "/foo/"))
    (is-true (cl-ppcre:scan matcher "/foo/bar/car.txt"))
    (is-false (cl-ppcre:scan matcher "/blah/foo/"))
    (is-false (cl-ppcre:scan matcher "/blah/foo"))))

(defclass my-plugin (hex:acceptor-plugin)
  ())

(test register-plugin-only-registers-once
  (let ((acceptor (make-instance 'acceptor-with-plugins)))
    (hex:register-plugin acceptor 'my-plugin :prefix "/foo/")
    (assert-that (hex:acceptor-plugins acceptor)
                 (has-length 1))
    (hex:register-plugin acceptor 'my-plugin :prefix "/foo/")
    (assert-that (hex:acceptor-plugins acceptor)
                 (has-length 1))))

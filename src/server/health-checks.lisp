(defpackage :server/health-checks
  (:use #:cl
        #:util/health-check)
  (:import-from #:util/health-check
                #:def-health-check)
  (:local-nicknames (#:a #:alexandria)))
(in-package :server/health-checks)

(def-health-check test-compile-file-pathname ()
  (uiop:with-temporary-file (:pathname p :type "lisp")
    (assert (equal "64ufasl"
                   (pathname-type (compile-file-pathname p))))))

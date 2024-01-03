;; Copyright 2019 Modern Interpreters Inc.

(defpackage #:hunchentoot-multi-acceptor
  (:use #:cl
        #:hunchentoot)
  (:export #:multi-acceptor
           #:*default-acceptor*
           :listen-fd
           #:add-acceptor
           #:default-acceptor
           #:*top-level-acceptor*))

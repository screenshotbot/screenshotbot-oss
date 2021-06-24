;; Copyright 2019 Modern Interpreters Inc.

(asdf:defsystem #:hunchentoot-multi-acceptor
  :description "Multiple domain support under single hunchentoot acceptor"
  :author "Arnold Noronha <arnold@jipr.io>"
  :license  "Apache License, Version 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot
               #:usocket
               #:alexandria
               #:str)
  :components ((:file "package")
               (:file "hunchentoot-multi-acceptor")))

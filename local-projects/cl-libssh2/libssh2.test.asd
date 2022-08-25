;;; -*- mode: lisp; syntax: common-lisp; indent-tabs-mode: nil -*-

(in-package :asdf)

(defsystem :libssh2.test
  :description "cl-libssh2 tests"
  :author       "Oleksii Shevchuk <alxchk@gmail.com>"
  :license      "Public Domain"
  :depends-on   (#:libssh2
                 #:hu.dwim.stefil)
  :serial       t
  :components   ((:module "test"
                  :components
                  ((:file "package")
                   (:file "scp")
                   (:file "sftp")))))

(defmethod perform ((o test-op) (c (eql (find-system :libssh2.test))))
  (funcall (intern (string '#:run-unit-tests) '#:libssh2.test)))

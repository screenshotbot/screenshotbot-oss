;; -*-Lisp-*-

(in-package :cl-user)

(defpackage :bknr.datastore.system
  (:use :cl :asdf))

(in-package :bknr.datastore.system)

(defsystem :bknr.datastore
  :name "baikonour datastore"
  :author "Hans Huebner <hans@huebner.org>"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :version "0"
  :maintainer "Hans Huebner <hans@huebner.org>"
  :licence "BSD"
  :description "baikonour - launchpad for lisp satellites"

  :depends-on (:cl-interpol
               :closer-mop
               :alexandria
               :unit-test
               :bknr.utils
               :bknr.indices
               :yason
               :trivial-utf-8
               #+sbcl :sb-posix
               #+lispworks :float-features
               #+lispworks :cffi)

  :components ((:module "data" :components ((:file "package")
                                            (:file "encoding" :depends-on ("package"))
                                            (:file "txn" :depends-on ("encoding" "package"))
                                            (:file "object" :depends-on ("txn" "package"))
                                            (:file "object-tests" :depends-on ("object" "package"))
                                            (:file "json" :depends-on ("object"))
                                            (:file "blob" :depends-on ("txn" "object" "package"))))))

(defsystem :bknr.datastore.test
  :depends-on (:bknr.datastore :fiveam :cl-store :bknr.utils)
  :components ((:module "data" :components ((:file "encoding-test")
                                            (:file "object-tests")))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :bknr.datastore))))
  (asdf:oos 'asdf:load-op :bknr.datastore.test)
  (eval (read-from-string "(it.bese.fiveam:run! :bknr.datastore)")))

(in-package :cl-user)

(defpackage :bknr.skip-list.system
  (:use :cl :asdf))

(in-package :bknr.skip-list.system)

(defsystem :bknr.skip-list
    :name "skip-list"
    :author "Manuel Odendahl <manuel@bl0rg.net>"
    :version "0"
    :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
    :licence "BSD"
    :description "Skiplist implementation for bknr"

    :components ((:module "skip-list" :components
			  ((:file "package")
			   (:file "skip-list" :depends-on ("package"))))))

(defsystem :bknr.skip-list.test
  :depends-on (:unit-test :bknr.skip-list) 
  :components ((:module "skip-list" :components
                        ((:file "skip-list-tests")))))



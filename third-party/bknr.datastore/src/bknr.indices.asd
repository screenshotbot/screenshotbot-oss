;;;; -*- Mode: LISP -*-

(in-package :cl-user)

(defpackage :bknr.indices.system
  (:use :cl :asdf))

(in-package :bknr.indices.system)

(defsystem :bknr.indices
  :name "bknr indices"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :version "0"
  :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
  :licence "BSD"
  :description "CLOS class indices"
  :long-description ""

  :depends-on (:cl-interpol :bknr.utils :bknr.skip-list :closer-mop)

  :components ((:module "indices"
			:components
			((:file "package")
			 (:file "protocol" :depends-on ("package"))
			 (:file "indices" :depends-on ("package" "protocol"))
			 (:file "indexed-class" :depends-on ("package" "indices"))
			 (:file "category-index" :depends-on ("package" "protocol" "indices"))))))

(defsystem :bknr.indices.test  
  :depends-on (:bknr.indices)
  :components ((:module "indices"
                        :components ((:file "indices-tests")))))


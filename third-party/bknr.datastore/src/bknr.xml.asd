;; -*-Lisp-*-

(in-package :cl-user)

(defpackage :bknr.xml.system
  (:use :cl :asdf))

(in-package :bknr.xml.system)

(defsystem :bknr.xml
    :name "baikonour"
    :author "Hans Huebner <hans@huebner.org>"
    :author "Manuel Odendahl <manuel@bl0rg.net>"
    :version "0"
    :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
    :licence "BSD"
    :description "baikonour - launchpad for lisp satellites"
    :depends-on (:cl-interpol :cxml)
    :components ((:module "xml" :components ((:file "package")
					     (:file "xml" :depends-on ("package"))))))


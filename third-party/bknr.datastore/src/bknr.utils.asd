;; -*-Lisp-*-

(in-package :cl-user)

(defpackage :bknr.utils.system
  (:use :cl :asdf))

(in-package :bknr.utils.system)

(defsystem :bknr.utils
    :name "baikonour"
    :author "Hans Huebner <hans@huebner.org>"
    :author "Manuel Odendahl <manuel@bl0rg.net>"
    :version "0"
    :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
    :licence "BSD"
    :description "baikonour - launchpad for lisp satellites"

    :depends-on (:cl-interpol :cl-ppcre
			      :md5
                              :flexi-streams
                              :alexandria
                              :bordeaux-threads)

    :components ((:module "statistics" :components ((:file "package")
						    (:file "runtime-statistics" :depends-on ("package"))))

		 (:module "utils" :components ((:file "package")
					       (:file "utils" :depends-on ("package"))
					       (:file "class" :depends-on ("package" "utils"))
					       #+(or cmu allegro openmcl sbcl)
					       (:file "smbpasswd" :depends-on ("utils"))
					       (:file "actor" :depends-on ("utils" "acl-mp-compat"))
					       (:file "reader" :depends-on ("utils"))
					       (:file "crypt-md5" :depends-on ("utils"))
					       (:file "capability" :depends-on ("utils"))
					       (:file "make-fdf-file" :depends-on ("utils"))
					       (:file "date-calc")
					       (:file "parse-time")
					       (:file "acl-mp-compat" :depends-on ("package"))))))


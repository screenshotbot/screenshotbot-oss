(in-package :cl-user)

(defpackage :bknr.impex
  (:use :cxml
        :closer-common-lisp
        :bknr.utils
        :bknr.xml
        :bknr.indices)
  #+cmu
  (:shadowing-import-from :common-lisp #:subtypep #:typep)

  (:export #:xml-class
	   #:parse-xml-file
	   #:write-to-xml
	   #:xml-class-importer

           #:with-xml-export
           #:with-xml-export*
           #:write-to-xml

	   #:create-instance
	   #:set-slot-value))

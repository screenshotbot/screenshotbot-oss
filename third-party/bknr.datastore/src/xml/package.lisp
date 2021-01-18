(in-package :cl-user)

(defpackage :bknr.xml
  (:use :cl
	:cl-ppcre
        :cl-interpol
	:cxml-xmls)
  (:shadowing-import-from :cl-interpol "QUOTE-META-CHARS")
  (:export
   #:node-children-nodes
   #:find-child
   #:find-children
   #:node-string-body
   #:node-attribute
   #:node-child-string-body
   #:node-to-html))

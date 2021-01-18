(in-package :cl-user)

(defpackage :bknr.indices
  (:use :closer-common-lisp
        :bknr.utils
	:bknr.skip-list)
  (:export #:index-add
	   #:index-get
	   #:index-remove
	   #:index-keys
	   #:index-values
	   #:index-reinitialize
	   #:index-clear
	   #:index-create
	   #:index-mapvalues
	   #:index-existing-error

	   #:initialize-indexed-instance
	   #:*initialized-indexed-instance*
	   #:*indexed-class-override*

	   #:slot-index
	   #:unique-index
	   #:string-unique-index
	   #:hash-index
	   #:hash-list-index
	   #:array-index
	   #:class-index
	   #:skip-list-index
	   #:class-skip-index

	   #:clear-class-indices
	   #:clear-slot-indices
	   #:class-slot-indices
	   #:class-slot-index

	   #:indexed-class
	   #:indexed-class-index-named
	   #:index-direct-slot-definition
	   #:index-effective-slot-definition
	   #:destroy-object
	   #:object-destroyed-p

	   #:category-index
	   #:tree-find-children
	   #:tree-find-siblings
	   #:parent-category
	   #:parent-categories
	   #:tree-categories))

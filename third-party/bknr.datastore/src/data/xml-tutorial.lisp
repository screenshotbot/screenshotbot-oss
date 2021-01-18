(in-package :bknr.datastore)

(make-instance 'mp-store :directory "/tmp/object-store/"
                         :subsystems (list
                                      (make-instance 'store-object-subsystem)))

(defvar *tutorial-dtd*
  (cxml:parse-dtd-file "xml-impex/tutorial.dtd"))

(defclass book (store-object)
  ((author :initarg :author :reader book-author
           :element "author")
   (id :initarg :id :reader book-id :type integer
       :attribute "id" :parser #'parse-integer)
   (isbn :initarg :isbn :reader book-isbn
         :attribute "isbn")
   (title :initarg :title :reader book-title
          :element "title"))
  (:metaclass persistent-xml-class)
  (:dtd *tutorial-dtd*)
  (:element "book"))

(bknr.impex:parse-xml-file "xml-impex/tutorial.xml" (list (find-class 'book))
                           :importer-class 'persistent-xml-class-importer)
(bknr.impex:write-to-xml (all-store-objects) :name "books")

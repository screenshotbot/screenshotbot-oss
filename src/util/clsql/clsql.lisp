(defpackage :util/clsql/clsql
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/clsql/clsql)

(setf clsql:*default-caching* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (asdf:system-relative-pathname
            :util/clsql
            "clsql/")
           clsql:*foreign-library-search-paths*
           :test #'equal))

(eval-when (:compile-toplevel)
  (asdf:compile-system :clsql-mysql)
  (asdf:compile-system :clsql-sqlite3))

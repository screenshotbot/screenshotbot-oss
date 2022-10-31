(defpackage :util/clsql/clsql
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/clsql/clsql)

(pushnew (asdf:system-relative-pathname
          :util/clsql
          "clsql/")
         clsql:*foreign-library-search-paths*
         :test #'equal)

(setf clsql:*default-caching* nil)

(defpackage :util/clsql/clsql
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/clsql/clsql)

(setf clsql:*default-caching* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((%push (path)
           (pushnew path
                    clsql:*foreign-library-search-paths*
                    :test #'equal)))
    (%push (asdf:system-relative-pathname
             :util/clsql
             "clsql/"))

    ;; Only for Homebrew on Mac. Technically only for ARM64.
    (%push #p"/opt/homebrew/opt/mysql-client/lib/")))

#-(or screenshotbot-oss windows)
(eval-when (:compile-toplevel)
  (asdf:compile-system :clsql-mysql)
  (asdf:compile-system :clsql-sqlite3))

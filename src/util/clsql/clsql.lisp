(defpackage :util/clsql/clsql
  (:use #:cl)
  #+lispworks
  (:import-from #:deliver-utils/common
                #:guess-root)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/clsql/clsql)

(setf clsql:*default-caching* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun update-search-path ()
    (flet ((%push (path)
             (pushnew path
                      clsql:*foreign-library-search-paths*
                      :test #'equal)))
      (%push
       (cond
         #+lispworks
         ((hcl:delivered-image-p)
          (path:catdir (guess-root) "util/clsql/clsql/"))
         (t
          (asdf:system-relative-pathname
           :util/clsql
           "clsql/"))))

      ;; Only for Homebrew on Mac. Technically only for ARM64.
      (%push #p"/opt/homebrew/opt/mysql-client/lib/")))

  (update-search-path))

#-(or screenshotbot-oss windows)
(eval-when (:compile-toplevel)
  (asdf:compile-system :clsql-mysql)
  (asdf:compile-system :clsql-sqlite3))

#+lispworks
(unless (hcl:delivered-image-p)
  (lw:define-action "Delivery Actions" "Make immutable systems"
    (lambda ()
      (asdf:register-immutable-system :clsql-mysql)
      (asdf:register-immutable-system :clsql-sqlite3))))

#+lispworks
(lw:define-action "When starting image" "Update search paths for mysql"
  #'update-search-path)

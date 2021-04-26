(defpackage :util-system
  (:use :cl
   :asdf))
(in-package :util-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (unless (find-package 'uffi)
     (asdf:operate 'asdf:load-op 'uffi)))

(defclass lib-source-file (c-source-file)
  ())

(defparameter *library-file-dir*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*))

(defmethod output-files ((o compile-op) (c lib-source-file))
  (let ((library-file-type
          (funcall (intern (symbol-name'#:default-foreign-library-type)
                           (symbol-name '#:uffi)))))
    (list (make-pathname :name (component-name c)
                         :type library-file-type
                         :defaults *library-file-dir*))))

(defmethod perform ((o load-op) (c lib-source-file))
  t)

(defmethod perform ((o compile-op) (c lib-source-file))
  (uiop:run-program (list "/usr/bin/gcc" "-shared" "-o" (namestring (car (output-files o c)))
                          "-Werror"
                          "-fPIC"
                          (namestring
                           (merge-pathnames (format nil "~a.c" (component-name c))
                                            *library-file-dir*)))
                    :output :interactive
                    :error-output :interactive))



(defsystem "util"
  :depends-on ("hunchentoot"
               "markup"
               "alexandria"
               "sentry"
               "drakma"
               "tmpdir"
               "cl-mop"
               "secure-random"
               "cl-base32"
               "parse-declarations-1.0"
               "bknr.datastore"
               "cl-csv"
               "cl-smtp"
               "cl-mongo-id"
               "cl-json"
               "hunchentoot-extensions"
               "net.mfiano.lisp.stripe"
               "closer-mop"
               "log4cl"
               "cl-cron"
               "ironclad")
  :serial t
  :components ((:file "package")
               (:file "mockable")
               (:file "olark")
               (:file "object-id")
               (:file "asdf")
               (:file "lists")
               (:file "payment-method")
               (:file "cdn")
               (lib-source-file "store-native")
               (:file "file-lock")
               (:file "store")
               (:file "countries")
               (:file "bind-form")
               (:file "google-analytics")
               (:file "misc")
               (:file "mail")
               (:file "download-file")
               (:file "html2text")
               (:file "testing")
               (:file "uuid")
               (:file "acceptor")
               (:file "mquery")))

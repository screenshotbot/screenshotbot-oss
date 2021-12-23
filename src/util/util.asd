(defpackage :util-system
  (:use :cl
   :asdf))
(in-package :util-system)

(defclass lib-source-file (c-source-file)
  ())

(defparameter *library-file-dir*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*))

(defun default-foreign-library-type ()
  "Returns string naming default library type for platform"
  #+(or win32 win64 cygwin mswindows windows) "dll"
  #+(or macosx darwin ccl-5.0) "dylib"
  #-(or win32 win64 cygwin mswindows windows macosx darwin ccl-5.0) "so"
)

(defmethod output-files ((o compile-op) (c lib-source-file))
  (let ((library-file-type
          (default-foreign-library-type)))
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
               "uuid"
               "clues"
               "tmpdir"
               "cl-mop"
               "secure-random"
               "cl-base32"
               "parse-declarations-1.0"
               "bknr.datastore"
               "cl-csv"
               "cl-smtp"
               "cl-mongo-id"
               "drakma"
               "cl-json"
               "hunchentoot-extensions"
               #-screenshotbot-oss
               "stripe"
               "log4cl"
               "cl-cron")
  :serial t
  :components ((:file "random-port")
               (:file "ret-let")
               (:file "copying")
               (:file "make-instance-with-accessors")
               (:file "emacs")
               (:file "cookies")
               (:file "package")
               (:file "mockable")
               (:file "object-id")
               (:file "asdf")
               (:file "lists")
               #-screenshotbot-oss
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
               (:file "mquery")
               (:file "form-errors")
               (:file "debugger-hook")))

(defsystem :util/fiveam
  :depends-on (:util
               :fiveam
               :pkg
               :cl-mock
               :str)
  :serial t
  :components ((:file "fiveam")
               (:file "mock-recording")))

(defsystem :util/phabricator
  :depends-on (:dexador
               :alexandria
               :cl-json
               :pkg
               :quri)
  :components ((:module "phabricator"
                :components ((:file "conduit")))))

(defsystem :util/tests
  :depends-on (:util
               :util/fiveam)
  :serial t
  :components ((:module "tests"
                :components ((:file "test-package")
                             (:file "test-ret-let")
                             (:file "test-store")
                             (:file "test-cookies")
                             (:file "test-fiveam")
                             (:file "test-lists")
                             (:file "test-models")
                             (:file "test-cdn")
                             (:file "test-bind-form")
                             (:file "test-objectid")
                             (:file "test-file-lock")
                             (:file "test-html2text")
                             (:file "test-mockable")
                             (:file "test-mquery")
                             (:file "test-make-instance-with-accessors")))))

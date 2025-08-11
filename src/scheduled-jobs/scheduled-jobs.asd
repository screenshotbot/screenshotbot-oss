(defpackage :scheduled-jobs/scheduled-jobs.asd
  (:use #:cl
        #:asdf)
  (:local-nicknames (#:a #:alexandria)))
(in-package :scheduled-jobs/scheduled-jobs.asd)

(defclass lib-source-file (c-source-file)
  ((extra-args :initarg :extra-args
               :initform nil
               :reader extra-args)))

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
                         :defaults (asdf:component-pathname c)))))

(defmethod perform ((o load-op) (c lib-source-file))
  t)

(defmethod perform ((o compile-op) (c lib-source-file))
  (let ((output-file (car (output-files o c))))
    (restart-case
        (uiop:delete-file-if-exists output-file)
      (ignore-deletion ()
        (values)))
    (uiop:with-staging-pathname (output-file output-file)
      (uiop:run-program (list* "gcc" "-shared"
                               "-o" (namestring output-file)
                               "-Werror"
                               "-fPIC"
                               (namestring
                                (asdf:component-pathname c))
                               (extra-args c))
                        :output t
                        :error-output t))))

(defsystem #:scheduled-jobs/headers
  :serial t
  :components ((static-file "ccronexpr" :type "h")))

(defsystem #:scheduled-jobs
  :serial t
  :depends-on (#:cl-cron
               #:scheduled-jobs/headers
               #:util/throttler
               #:util/native-module
               #:util
               #:util/health-check
               #:util.threading)
  :components ((lib-source-file "ccronexpr")
               (:file "model")
               (:file "bindings")
               (:file "scheduled-jobs")))

(defsystem #:scheduled-jobs/tests
  :serial t
  :depends-on (#:scheduled-jobs
               #:fiveam
               #:util/fiveam)
  :components ((:file "test-scheduled-jobs")
               (:file "test-bindings")))

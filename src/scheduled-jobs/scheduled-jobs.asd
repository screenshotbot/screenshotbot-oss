(defpackage :scheduled-jobs/scheduled-jobs.asd
  (:use #:cl
        #:asdf)
  (:local-nicknames (#:a #:alexandria)))
(in-package :scheduled-jobs/scheduled-jobs.asd)

(defclass lib-source-file (c-source-file)
  ((extra-args :initarg :extra-args
               :initform nil
               :reader extra-args)))

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
  (let ((output (output-file 'compile-op c)))
    #+nil
    (fli:disconnect-module :ccronexpr)
    #+lispworks
    (fli:register-module
     :ccronexpr
     :real-name output)
    #-lispworks
    (cffi:load-foreign-library output)))

(defmethod perform ((o compile-op) (c lib-source-file))
  (uiop:run-program (list* "gcc" "-shared"
                           "-o" (namestring (car (output-files o c)))
                           "-Werror"
                           "-fPIC"
                           (namestring
                            (merge-pathnames (format nil "~a.c" (component-name c))
                                             *library-file-dir*))
                           (extra-args c))
                    :output t
                    :error-output t))

(defsystem #:scheduled-jobs/headers
  :serial t
  :components ((static-file "ccronexpr" :type "h")))

(defsystem #:scheduled-jobs
  :serial t
  :depends-on (#:cl-cron
               #:scheduled-jobs/headers
               #:util
               #:util/threading
               #:priority-queue)
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

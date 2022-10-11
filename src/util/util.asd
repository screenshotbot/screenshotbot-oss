(defpackage :util-system
  (:use :cl
   :asdf))
(in-package :util-system)

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
  t)

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



(defsystem "util"
  :depends-on ("hunchentoot"
               "markup"
               "nibble"
               "alexandria"
               "uuid"
               "clues"
               "tmpdir"
               "cl-mop"
               "secure-random"
               "cl-base32"
               "bknr.datastore"
               "cl-csv"
               "cl-smtp"
               "util/cron"
               "cl-mongo-id"
               "drakma"
               "cl-json"
               "hunchentoot-extensions"
               #-screenshotbot-oss
               "stripe"
               "log4cl"
               "util/threading"
               "util/store"
               "util/misc"
               "util/random-port"
               "cl-cron")
  :serial t
  :components ((:file "ret-let")
               (:file "copying")
               (:file "make-instance-with-accessors")
               (:file "emacs")
               (:file "cookies")
               (:file "bind-form")
               (:file "object-id")
               (:file "lists")
               (:file "html2text")
               (:file "cdn")
               (:file "package")
               (:file "mockable")
               (:file "asdf")
               #-screenshotbot-oss
               (:file "payment-method")
               (:file "countries")
               (:file "google-analytics")
               (:file "mail")
               (:file "download-file")
               (:file "uuid")
               (:file "acceptor")
               (:file "mquery")
               (:file "form-errors")
               (:file "debugger-hook")
               #+lispworks
               (:file "memory")))

(defsystem :util/logrotate
  :serial t
  :depends-on ()
  :components ((:file "logrotate")))

(defsystem :util/misc
  :serial t
  :depends-on (:local-time
               :str)
  :components ((:file "misc")))

#+lispworks
(defsystem :util/remote-debugging
  :serial t
  :depends-on ((:require "remote-debugger-client"))
  :components ((:file "remote-debugging")))

(defsystem :util/testing
  :serial t
  :depends-on (:fiveam
               :util)
  :components ((:file "testing")))

(defsystem :util/random-port
  :serial t
  :depends-on (:usocket)
  :components ((:file "random-port")))

(defsystem :util/form-state
  :serial t
  :depends-on (:closer-mop
               :alexandria)
  :components ((:file "form-state")))

(defsystem :util/store
  :serial t
  :depends-on (:bknr.datastore
               :util/misc
               :str
               :local-time
               :copy-directory
               :ironclad/core
               :cffi
               :cl-cron)
  :components (#-mswindows (lib-source-file "store-native")
		#-mswindows
               (:file "file-lock")
               (:file "store")))

(defsystem :util/lru-cache
  :serial t
  :depends-on (:easy-macros
               :trivial-file-size
               :cl-fad)
  :components ((:file "lru-cache")))

(defsystem :util/bknr-slynk
  :serial t
  :depends-on (:slynk)
  :components ((:file "bknr-slynk")))

(defsystem :util/fiveam
  :depends-on (:util/testing
               :fiveam
               :pkg
               :cl-mock
               :str)
  :serial t
  :components ((:file "fiveam")
               (:file "mock-recording")))

(defsystem :util/digests
  :depends-on ((:feature (:not (:and :lispworks (:or :linux :darwin))) "md5")
               (:feature (:not (:and :lispworks (:or :linux :darwin))) "ironclad"))
  :serial t
  :components ((lib-source-file
                "digest"
                :extra-args
                (#+darwin "-I/opt/homebrew/opt/openssl/include/"
                 #+mswindows "-IC:\\Program Files\\OpenSSL-Win64\\include\\")
                :if-feature (:and :lispworks (:or :linux :darwin :mswindows)))
               (:file "digests" :if-feature (:and :lispworks (:or :linux :darwin :mswindows)))
               (:file "digests-non-lw" :if-feature (:not (:and :lispworks (:or :linux :darwin :mswindows))))))

(defsystem :util/threading
  :depends-on (:bordeaux-threads
               :mailbox
               #-screenshotbot-oss
               :sentry
               :trivial-garbage
               :easy-macros
               :util/misc)
  :serial t
  :components ((:file "fake-mp")
               (:file "threading")))

(defsystem :util/cron
  :depends-on (:cl-cron
               :util/threading)
  :components ((:file "cron")))

(defsystem :util/request
  :depends-on (:drakma
               :easy-macros)
  :serial t
  :components ((:file "request")))

(defsystem :util/health-check
  :depends-on (:str
               :alexandria)
  :serial t
  :components ((:file "health-check")))

(defsystem :util/hash-lock
  :depends-on (:bordeaux-threads
               :lparallel
               :alexandria)
  :serial t
  :components ((:file "hash-lock")))

(defsystem :util/fake-fli
  :depends-on (:cffi
               :str)
  :serial t
  :components (#-lispworksc (:file "fake-fli")))

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
               :util/hash-lock
               :util/health-check
               :util/lru-cache
               :fiveam-matchers
               :easy-macros
               :util/digests
               :util/request
               :util/fiveam)
  :serial t
  :components ((:module "tests"
                :components ((:file "test-ret-let")
                             (:file "test-misc")
                             (:file "test-request")
                             (:file "test-store")
                             (:file "test-hash-lock")
                             (:file "test-threading")
                             (:file "test-cookies")
                             (:file "test-fiveam")
                             (:file "test-testing")
                             (:file "test-lists")
                             (:file "test-models")
                             (:file "test-cdn")
                             (:file "test-bind-form")
                             (:file "test-lru-cache")
                             (:file "test-objectid")
			     #-mswindows
                             (:file "test-file-lock")
                             #-(or darwin mswindows)
                             (:file "test-html2text")
                             (:file "test-mockable")
                             (:file "test-health-check")
                             (:file "test-mquery")
                             (:file "test-make-instance-with-accessors")
                             (:file "test-digests")))))

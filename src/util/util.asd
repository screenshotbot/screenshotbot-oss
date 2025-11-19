(defpackage :util-system
  (:use :cl
   :asdf))
(in-package :util-system)

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
  (uiop:run-program (list* "gcc" "-shared"
                           "-o" (namestring (car (output-files o c)))
                           "-Werror"
                           "-fPIC"
                           (namestring
                            (asdf:component-pathname c))
                           (extra-args c))
                    :output t
                    :error-output t))



(defsystem "util"
  :depends-on ("hunchentoot"
               "markup"
               "nibble"
               "easy-macros"
               "alexandria"
               "uuid"
               "tmpdir"
               "cl-mop"
               "secure-random"
               "cl-base32"
               "bknr.datastore"
               "cl-csv"
               "cl-smtp"
               "util/cron"
               "drakma"
               "cl-json"
               "hunchentoot-extensions"
               #-(or screenshotbot-oss eaase-oss)
               "stripe"
               "log4cl"
               "util.threading"
               "util.store"
               "util.misc"
               "util/html2text"
               "util/random-port"
               (:feature (:and :lispworks (:or :darwin :linux)) "util/memory")
               "cl-cron")
  :serial t
  :components ((:file "ret-let")
               (:file "copying")
               (:file "make-instance-with-accessors")
               (:file "emacs")
               (:file "cookies")
               (:file "bind-form")
               (:file "cdn")
               (:file "package")
               (:file "mockable")
               (:file "asdf")
               #- (or screenshotbot-oss eaase-oss)
               (:file "payment-method")
               (:file "countries")
               (:file "google-analytics")
               (:file "mail")
               (:file "download-file")
               (:file "uuid")
               (:file "acceptor")
               (:file "mquery")
               (:file "form-errors")
               (:file "debugger-hook")))

(defsystem :util/copy-file
  :serial t
  :defsystem-depends-on (:trivial-features)
  :depends-on (:uiop
               #-lispworks
               :util/fake-fli
               :util/posix)
  :components ((:file "copy-file")))

(defsystem :util/timeago
  :serial t
  :depends-on (:markup
               :local-time)
  :components ((:file "timeago")))

(defsystem :util/html2text
  :serial t
  :depends-on (:html2text
               :markup)
  :components ((:file "html2text")))

#+lispworks
(defsystem :util/memory
    :serial t
  :depends-on (:util/posix)
  :components ((:file "memory")))

(defsystem :util/logrotate
  :serial t
  :depends-on ()
  :components ((:file "logrotate")))


(defsystem :util/atomics
  :serial t
  :depends-on (:atomics)
  :components ((:file "atomics")))

#+lispworks
(defsystem :util/remote-debugging
  :serial t
  :depends-on ((:require "remote-debugger-client"))
  :components ((:file "remote-debugging")))


(defsystem :util/random-port
  :serial t
  :depends-on (:usocket
               :util.misc
               :easy-macros)
  :components ((:file "random-port")))

(defsystem :util/form-state
  :serial t
  :depends-on (:closer-mop
               :alexandria)
  :components ((:file "form-state")))


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
  :depends-on (:fiveam
               :pkg
               :cl-mock
               :log4cl
               :cl-store
               :str)
  :serial t
  :components ((:file "fiveam")
               (:file "mock-recording")))

(defsystem :util/posix
  :depends-on (#-lispworks
               :util/fake-fli)
  :components ((:file "posix")))

(defsystem :util/digests
  :depends-on ((:feature (:not (:and :lispworks (:or :linux))) "md5")
               (:feature (:not (:and :lispworks (:or :linux))) "ironclad")
               :util/health-check
               :easy-macros)
  :serial t
  :components ((lib-source-file
                "digest"
                :if-feature (:and :lispworks (:or :linux)))
               (:file "digests" :if-feature (:and :lispworks (:or :linux)))
               (:file "digests-non-lw" :if-feature (:not (:and :lispworks (:or :linux))))))

(defsystem :util/sizeof
  :serial t
  :depends-on (#-lispworks
               :util/fake-fli)
  :components ((:file "sizeof")))


(defsystem :util/cron
  :depends-on (:cl-cron
               :bknr.datastore
               :util.threading)
  :components ((:file "cron")))

(defsystem :util/request
  :depends-on (:drakma
               :str
               :log4cl
               :quri
               :anaphora
               :trivial-gray-streams
               :easy-macros)
  :serial t
  :components ((:file "http-cache")
               (:file "truncated-stream")
               (:file "request")
               (:file "engines")
               (:file "reused-ssl")))

(defsystem :util/hunchentoot-engine
  :depends-on (:util/request
               :hunchentoot)
  :serial t
  :components ((:file "hunchentoot-engine")))

(defsystem :util/disk-size
  :depends-on (:util/sizeof)
  :components ((:file "disk-size")))

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

(defsystem :util/lparallel
  :depends-on (:lparallel
               :util.misc
               :easy-macros)
  :serial t
  :components ((:file "lparallel")))

(defsystem :util/native-module
  :depends-on (:util/fake-fli
               :util.misc)
  :components ((:file "native-module")))

(defsystem :util/fake-fli
  :depends-on (:cffi
               :str)
  :serial t
  :components (#-lispworksc (:file "fake-fli")))

(defsystem :util/phabricator
  :depends-on (:dexador
               :alexandria
               :cl-json
               :fset
               :json-mop
               :pkg
               :util/request
               :util.misc
               :quri)
  :components ((:module "phabricator"
                :components ((:file "conduit")
                             (:file "harbormaster")
                             (:file "project")
                             (:file "maniphest")
                             (:file "passphrase")))))

(defsystem :util/clsql
  :depends-on (:clsql
               (:feature :lispworks :deliver-utils)
               :trivial-features)
  :components ((:module "clsql"
                :components ((:file "clsql")))))

(defsystem :util/logger
  :depends-on (:log4cl
               :easy-macros
               :alexandria
               :bordeaux-threads)
  :components ((:file "logger")))

(defsystem :util/json-mop
  :depends-on (:json-mop)
  :serial t
  :components ((:file "json-mop")))

(defsystem :util/throttler
  :depends-on (:local-time
               :fset
               :atomics
               :easy-macros
               :cl-fad)
  :serial t
  :components ((:file "throttler")))

(defsystem :util/simple-queue
  :depends-on ()
  :components ((:file "simple-queue")))

(defsystem :util/tests
  :depends-on (:util
               :util/simple-queue
               :util/hunchentoot-engine
               :util/hash-lock
               :util/lparallel
               :util/health-check
               :util/gcloud
               :util/lru-cache
               :util/copy-file
               :fiveam-matchers
               :easy-macros
               :util/digests
               :util/sizeof
               :util/disk-size
               :util/phabricator
               :util/logger
               :util/timeago
               :util/json-mop
               :util/throttler
               :util.testing
               :util/events
               :util/request
               :util/fake-clingon
               :util/fiveam
               :util/benchmark
               :util/fset
               (:feature :lispworks :util/ssl)
               :serapeum)
  :defsystem-depends-on (:trivial-features)
  :serial t
  :components ((:module "tests"
                :components ((:static-file "test-file" :type "txt")
                             (:static-file "test-file-compressed" :type "txt.gz")
                             (:file "test-ret-let")
                             (:file "test-mail")
                             (:file "test-hunchentoot-engine")
                             (:file "test-fset")
                             (:file "test-reused-ssl")
                             (:file "test-truncated-stream")
                             (:file "test-asdf")
                             (:file "test-fake-clingon")
                             (:file "test-throttler")
                             (:file "test-fake-fli" :if-feature (:not :lispworks))
                             (:file "test-logger")
                             (:file "test-copy-file")
                             (:file "test-request")
                             (:file "test-request-integration")
                             (:file "test-timeago")
                             (:file "test-random-port")
                             (:file "test-json-mop")
                             (:file "test-lparallel")
                             (:file "test-hash-lock")
                             (:file "test-cookies")
                             (:file "test-fiveam")
                             (:file "test-mock-recording")
                             (:file "test-sizeof")
                             (:file "test-models")
                             (:file "test-disk-size")
                             (:file "test-events" :if-feature (:not :windows))
                             (:file "test-cdn")
                             (:file "test-bind-form")
                             (:file "test-simple-queue")
                             (:file "test-lru-cache")
                             #-(or darwin mswindows)
                             (:file "test-html2text")
                             (:file "test-mockable")
                             (:file "test-health-check")
                             (:file "test-mquery")
                             #+ (and lispworks linux)
                             (:file "test-memory")
                             (:file "test-make-instance-with-accessors")
                             (:file "test-digests")
                             (:file "test-benchmark")
                             #+(and lispworks (not darwin) (not arm64))
                             (:file "test-ssl")
                             #+lispworks
                             (:file "test-lispworks")))
               (:module "phabricator"
                :components ((:file "test-conduit")
                             (:file "test-harbormaster")))))

(defsystem :util/events
  :serial t
  :defsystem-depends-on (:trivial-features)
  :depends-on (:clsql
               :util/clsql
               :util/cron
               :core.installation
               :util.misc
               :util/atomics
               :atomics)
  :components ((:file "events")))

(defsystem :util/gcloud
  :serial t
  :depends-on (:cl-mongo-id)
  :components ((:file "gcloud")))

#+lispworks
(defsystem :util/ssl
    :serial t
    :depends-on (:util.misc)
    :components ((:file "ssl")))

(defsystem :util/symbol-detector
  :serial t
  :depends-on (:markup
               :named-readtables)
  :components ((:file "symbol-detector")))

(defsystem :util/http-ping
  :serial t
  :depends-on (:util/request)
  :components ((:file "http-ping")))

(defsystem :util/fake-clingon
  :serial t
  :depends-on (:clingon)
  :components ((:file "fake-clingon")))


(defsystem :util/fset
  :serial t
  :depends-on (:fset
               :easy-macros)
  :components ((:file "fset")))

(defsystem :util/recaptcha
  :serial t
  :depends-on (:util/request
               :auto-restart)
  :components ((:file "recaptcha")))

(defsystem :util/benchmark
  :serial t
  :depends-on (:alexandria)
  :components ((:file "benchmark")))

(defsystem :util.store
  :serial t
  :depends-on (:bknr.datastore
               :util/misc
               :trivial-features
               :util/cron
               :file-lock
               :tmpdir
               :str
               (:feature (:and :lispworks :linux) :bknr.cluster)
               :easy-macros
               :auto-restart
               :util/threading
               :util/copy-file
               :util/events
               :local-time
               :atomics
               :util.store/encodable
               :alexandria
               :fset
               :cl-mongo-id
               :copy-directory
               :ironclad/core
               (:feature (:not :lispworks) :util/fake-fli)
               :cffi
               :cl-cron)
  :components ((:file "elb-store" :if-feature (:and :lispworks :linux))
               (:file "clone-logs-store" :if-feature (:and :lispworks :linux))
               (:file "store")
               (:file "store-version")
               (:file "object-id")
               (:file "single")
               (:file "migrations")
               (:file "delayed-accessors")
               (:file "checksums")
               (:file "export")
               (:file "fset")
               (:file "fset-index")
               (:file "permissive-persistent-class")
               (:file "store-migrations")
               (:file "validate")))

(defsystem :util.store/encodable
  :serial t
  :depends-on (:bknr.datastore)
  :components ((:file "encodable")))

(defsystem :util.store/tests
  :serial t
  :depends-on (:util.store
               :fiveam-matchers
               :util.store/aws
               :util.store/raft-state
               :util/fiveam)
  :components ((:file "test-store-version")
               (:file "test-store")
               (:file "test-objectid")
               (:file "test-migrations")
               (:file "test-delayed-accessors")
               (:file "test-raft-state-http" :if-feature (:and :lispworks :linux))
               (:file "test-checksums")
               (:file "test-fset")
               (:file "test-validate")
               (:file "test-fset-index")
               (:file "test-encodable")
               (:file "test-permissive-persistent-class")))

(defsystem :util.store/aws
  :serial t
  :depends-on (:util.store
               :util/request)
  :components ((:file "aws-store")))

(defsystem :util.store/raft-state
  :serial t
  :depends-on (:util.store
               :hunchentoot)
  :components ((:file "raft-state-http" :if-feature (:and :lispworks :linux))))

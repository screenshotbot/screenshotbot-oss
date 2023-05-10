(defsystem :util.store
  :serial t
  :depends-on (:bknr.datastore
               :util/misc
               :util/cron
               :util/file-lock
               :tmpdir
               :str
               :easy-macros
               :local-time
               :atomics
               :alexandria
               :fset
               :cl-mongo-id
               :copy-directory
               :ironclad/core
               (:feature (:not :lispworks) :util/fake-fli)
               :cffi
               :cl-cron)
  :components ((:file "store")
               (:file "store-version")
               (:file "object-id")
               (:file "single")
               (:file "migrations")
               (:file "delayed-accessors")
               (:file "checksums")
               (:file "export")
               (:file "fset-index")
               (:file "store-migrations")))

(defsystem :util.store/tests
  :serial t
  :depends-on (:util.store
               :fiveam-matchers
               :util/fiveam)
  :components ((:file "test-store-version")
               (:file "test-store")
               (:file "test-objectid")
               (:file "test-migrations")
               (:file "test-delayed-accessors")
               (:file "test-checksums")
               (:file "test-fset-index")))

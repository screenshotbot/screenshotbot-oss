(defsystem :util.store
  :serial t
  :depends-on (:bknr.datastore
               :util/misc
               :util/cron
               :tmpdir
               :str
               :easy-macros
               :local-time
               :fset
               :cl-mongo-id
               :copy-directory
               :ironclad/core
               (:feature (:not :lispworks) :util/fake-fli)
               :cffi
               :cl-cron)
  :components ((:file "file-lock")
               (:file "store")
               (:file "store-version")
               (:file "object-id")
               (:file "single")
               (:file "migrations")
               (:file "delayed-accessors")
               (:file "export")))

(defsystem :util.store/tests
  :serial t
  :depends-on (:util.store
               :util/fiveam)
  :components ((:file "test-file-lock"
                :if-feature (:and
                             :lispworks
                             (:not :windows)))
               (:file "test-store-version")
               (:file "test-store")
               (:file "test-objectid")
               (:file "test-migrations")
               (:file "test-delayed-accessors")))
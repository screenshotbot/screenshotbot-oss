(defsystem :scale
  :serial t
  :depends-on (:util/request
               :str
               :quri
               :alexandria
               :auto-restart
               :ironclad
               :md5
               :secure-random
               :hunchentoot
               :bknr.datastore
               :cl-json
               :tmpdir
               :bordeaux-threads
               :libssh2
               :cl-fad)
  :components ((:file "core")
               (:file "linode")
               (:file "vagrant")
               (:file "image")))

(defsystem #:scale/tests
  :serial t
  :depends-on (#:scale
               #:cl-mock
               #:util/fiveam)
  :components ((:file "test-core")))

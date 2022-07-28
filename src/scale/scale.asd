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
               :cl-json
               :tmpdir
               :bordeaux-threads
               :libssh2
               :cl-fad)
  :components ((:file "core")
               (:file "linode")
               (:file "vagrant")
               (:file "image")))

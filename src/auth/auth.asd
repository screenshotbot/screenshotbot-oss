(asdf:defsystem "auth"
    :serial t
  :depends-on ("cl-pass"
               "bknr.datastore"
               "util/misc"
               "core.installation"
               "util.store"
               "log4cl"
               "util/cron"
               "secure-random"
               "cl-intbytes"
               "cl-fad"
               "cl-store"
               "hunchentoot")
  :components ((:file "package")
               (:file "auth")
               (:file "viewer-context")
               (:file "request")
               (:file "view")
               (:file "company")
               (:file "acceptor")))

(defsystem "auth/tests"
  :serial t
  :depends-on (:auth
               :util/testing
               :util.store
               :core.api
               :fiveam-matchers
               :util/fiveam)
  :components ((:file "test-auth")
               (:file "test-view")))

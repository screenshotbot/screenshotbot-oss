(asdf:defsystem "auth"
    :serial t
  :depends-on ("cl-pass"
               "bknr.datastore"
               "util.misc"
               "util.statsig"
               "core.installation"
               "hunchentoot-extensions"
               "util.store"
               "log4cl"
               "util/cron"
               "secure-random"
               "gravatar"
               "cl-intbytes"
               "encrypt"
               "cl-fad"
               "cl-store"
               "hunchentoot")
  :components ((:file "package")
               (:file "auth")
               (:file "viewer-context")
               (:file "request")
               (:file "view")
               (:file "company")
               (:file "acceptor")
               (:file "avatar")))

(defsystem "auth/tests"
  :serial t
  :depends-on (:auth
               :util.testing
               :util.store
               :core.api
               :fiveam-matchers
               :util/fiveam)
  :components ((:file "test-auth")
               (:file "test-view")
               (:file "test-avatar")))

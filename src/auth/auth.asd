(asdf:defsystem "auth"
    :serial t
  :depends-on ("cl-pass"
               "bknr.datastore"
               "util/misc"
               "core.installation"
               "util.store"
               "log4cl"
               "cl-fad"
               "cl-store"
               "hunchentoot"
               "session-token")
  :components ((:file "package")
               (:file "auth")
               (:file "request")
               (:file "view")
               (:file "company")
               (:file "acceptor")))

(defsystem "auth/tests"
  :serial t
  :depends-on (:auth
               :util/testing
               :fiveam-matchers
               :util/fiveam)
  :components ((:file "test-auth")
               (:file "test-view")))

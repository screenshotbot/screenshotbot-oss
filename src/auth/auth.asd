(asdf:defsystem "auth"
    :serial t
  :depends-on ("cl-pass"
               "bknr.datastore"
               "util/misc"
               "log4cl"
               "cl-fad"
               #+windows
               "cl-store"
               "session-token")
  :components ((:file "package")
               (:file "auth")))

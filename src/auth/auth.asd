(asdf:defsystem "auth"
    :serial t
  :depends-on ("clsql"
               "util"
               "cl-pass"
               "log4cl"
               "cl-fad"
               "cl-store"
               "session-token")
  :components ((:file "package")
               (:file "auth")))

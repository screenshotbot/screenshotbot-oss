(asdf:defsystem "auth"
    :serial t
  :depends-on ("util"
               "cl-pass"
               "log4cl"
               "cl-fad"
               #+windows
               "cl-store"
               "session-token")
  :components ((:file "package")
               (:file "auth")))

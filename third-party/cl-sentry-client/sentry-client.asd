(asdf:defsystem #:sentry-client
  :description "Sentry client"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:drakma
               :cl-json
               :cl-ppcre
               :uuid
               :fmt
               :local-time
               :trivial-backtrace)
  :components ((:file "package")
               (:file "sentry-client")))

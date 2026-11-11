(asdf:defsystem #:sentry-client.hunchentoot
  :description "Hunchentoot integration for Sentry client"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:sentry-client
               :hunchentoot)
  :components ((:file "hunchentoot")))

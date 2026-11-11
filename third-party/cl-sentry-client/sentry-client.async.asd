(asdf:defsystem #:sentry-client.async
  :description "Asynchronous Sentry client"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:sentry-client
               :simple-tasks)
  :components ((:file "async")))

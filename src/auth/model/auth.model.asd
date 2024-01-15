(defsystem #:auth.model
  :serial t
  :depends-on (#:auth)
  :components ((:file "user")))

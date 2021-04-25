(defsystem :hunchentoot-extensions
  :serial t
  :depends-on (:hunchentoot
               :parse-declarations-1.0
               :do-urlencode
               :log4cl)
  :components ((:file "package")
               (:file "url")
               (:file "acceptor-with-plugins")
               (:file "better-easy-handler")))

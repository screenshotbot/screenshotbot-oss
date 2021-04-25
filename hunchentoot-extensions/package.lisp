(defpackage :hunchentoot-extensions
  (:nicknames :hex)
  (:use :cl
   :alexandria)
  (:export :base-acceptor
   :better-easy-handler
           :acceptor-funcall-handler
   :make-full-url
   :acceptor-plugin-name
           :acceptor-plugin-prefix
           :wrap-template
           :acceptor-plugin
   :define-plugin-handler
           :acceptor-with-plugins
           :*acceptor-plugin*
           :safe-domain
           :make-uri-regex
           :acceptor-db-config
           :make-uri-regex
   :add-get-param-to-url
   :acceptor-plugins
   :safe-redirect
   :make-url))

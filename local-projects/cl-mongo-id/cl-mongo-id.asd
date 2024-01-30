(asdf:defsystem :cl-mongo-id
  :author "Andrew Lyon <orthecreedence@gmail.com>"
  :description "A library for the creation/parsing of MongoDB Object IDs"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:bordeaux-threads :md5 :local-time
                                 :secure-random)
  :components ((:file "mongo-id")))

(asdf:defsystem :hunchensocket
  :description "WebSockets for Hunchentoot"
  :author "capitaomorte <https://github.com/capitaomorte>"
  :license "MIT"
  :version #.(with-open-file (f "VERSION") (string (read f)))
  :depends-on (:hunchentoot
               :alexandria
               :ironclad
               :flexi-streams
               :chunga
               :trivial-utf-8
               :trivial-backtrace
               :bordeaux-threads
               :cl-fad)
  :serial t
  :components
  ((:file "package")
   (:file "hunchensocket")))

(asdf:defsystem :hunchensocket-tests
  :description "Tests for Hunchensocket"
  :version #.(with-open-file (f "VERSION") (string (read f)))
  :depends-on (:fiasco
               :hunchensocket)
  :serial t
  :components
  ((:file "hunchensocket-tests")))



(defsystem :control-socket
  :serial t
  :depends-on (:unix-sockets
               :util/threading
               :log4cl)
  :components ((:file "server")))

(defsystem :control-socket/tests
  :serial t
  :depends-on (:control-socket
               :util/fiveam))

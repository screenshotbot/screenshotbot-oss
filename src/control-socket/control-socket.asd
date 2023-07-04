(defsystem :control-socket
  :serial t
  :depends-on (:unix-sockets
               :util/threading
               :log4cl
               (:feature :lispworks (:require "remote-debugger-full")))
  :components ((:file "server" :if-feature (:and :lispworks (:not :mswindows)))))

#+(and :lispworks (:not :mswindows))
(defsystem :control-socket/tests
  :serial t
  :depends-on (:control-socket
               :util/fiveam))

(defpackage :server/control-socket
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/store
                #:object-store)
  (:import-from #:control-socket/server
                #:control-socket-stop
                #:make-control-socket))
(in-package :server/control-socket)

(def-easy-macro with-control-socket  (&key store-dir &fn fn)
  (let* ((socket-file (ensure-directories-exist
                       (path:catfile (user-homedir-pathname)
                                     "sockets/"
                                     (format nil "~a.sock" (util/posix:getpid))))))

    (when (probe-file socket-file)
      (delete-file socket-file))

    (log:info "Start control-socket at ~a" socket-file)
    (let ((cs (make-control-socket socket-file)))
      (unwind-protect
           (funcall fn)
        (log:info "Shut down control socket")
        (control-socket-stop cs)))))

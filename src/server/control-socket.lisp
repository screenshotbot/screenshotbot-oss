(defpackage :server/control-socket
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/store
                #:object-store)
  (:import-from #:control-socket/server
                #:handle-request
                #:control-socket-stop
                #:make-control-socket))
(in-package :server/control-socket)


(def-easy-macro with-control-socket  (&fn fn)
  #+(or windows screenshotbot-oss)
  (funcall fn)
  #-(or windows screenshotbot-oss)
  (let* ((store-dir (object-store))
         (socket-file (ensure-directories-exist
                       (path:catfile store-dir "sockets/"
                                     (format nil "~a.sock" (osicat-posix:getpid))))))

    (when (probe-file socket-file)
      (delete-file socket-file))

    (log:info "Start control-socket at ~a" socket-file)
    (let ((cs (make-control-socket socket-file)))
      (unwind-protect
           (funcall fn)
        (log:info "Shut down control socket")
        (control-socket-stop cs)))))

(defmethod handle-request ((cmd (eql :snapshot)) &optional (message "snapshot name"))
  (util:safe-snapshot message))

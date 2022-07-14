(defpackage :util/remote-debugging
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:start-client-remote-debugging-server))
(in-package :util/remote-debugging)

(defun server-connect-callback (ssl password conn)
  (log:info "Incoming connection")
  (let ((stream (comm:create-ssl-socket-stream conn
                                               ssl
                                               :errorp t)))
    (let ((bytes (make-array 32 :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      (unless (equalp bytes (encode-password password))
        (error "Password mismatch, got ~s" bytes)))
    (log:info "Creating connection")
    (dbg:create-client-remote-debugging-connection "tdrhq-primary-connection"
                                                   :stream stream)))

(defun encode-password (password)
  (ironclad:digest-sequence :sha256 (flexi-streams:string-to-octets password) ))

(defun start-client-remote-debugging-server (&key port ssl password)
  (let ((server (comm:start-up-server
                 :function (a:curry #'server-connect-callback ssl password)
                 :service port)))
    server))

(defun connect-ide-remote-debugging-server (&key host port password)
  (let ((stream (comm:open-tcp-stream host port :direction :io :keepalive t
                                              :read-timeout nil
                                              :ssl-ctx t
                                              :write-timeout 300)))
    (write-sequence (encode-password password) stream)
    (dbg:create-ide-remote-debugging-connection "client"
                                                :stream stream)))

;; (setf *conn* (connect-ide-remote-debugging-server :host "localhost" :port  9090 :password "foobar"))
;; (dbg:ide-eval-form-in-remote `(progn system:*line-arguments-list*) :connection *conn*)

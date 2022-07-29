(defpackage :scale/core
  (:use #:cl)
  (:import-from #:libssh2
                #:authentication)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:create-instance
   #:delete-instance
   #:wait-for-ready
   #:with-instance
   #:scp
   #:encode-bash-command
   #:add-url
   #:ssh-run
   #:ip-address
   #:rewrite-known-hosts
   #:known-hosts
   #:secret-file
   #:ssh-sudo
   #:ssh-user
   #:ssh-port
   #:base-instance
   #:http-request-via
   #:with-cached-ssh-connections))
(in-package :scale/core)

(defvar *last-instance*)

(defvar *ssh-connection-cache*)

(defclass base-instance ()
  ((lock :initform (bt:make-lock)
         :reader lock)))

(defgeneric create-instance (provider type &key region))

(defgeneric delete-instance (instance))

(defgeneric wait-for-ready (instance))

(defgeneric ssh-run (instance cmd &key output error-output))

(defgeneric scp (instance local-file remote-file))

(defgeneric ip-address (instance))

(defgeneric known-hosts (instance))


(defmethod delete-instance :before ((instance base-instance)))

(defmacro with-known-hosts ((name) self &body body)
  (let ((self-sym (gensym "self"))
        (s (gensym "s")))
    `(let ((,self-sym ,self))
       (uiop:with-temporary-file (:pathname ,name :stream ,s :direction :output)
         (declare (ignorable ,name))
         (write-string (rewrite-known-hosts (known-hosts ,self-sym)
                                            (ip-address ,self-sym)) ,s)
         (finish-output ,s)
         ,@body))))

(defmethod ssh-connection ((self base-instance))
  (with-known-hosts (known-hosts) self
    (log:info "Creating ssh connection")
    (let ((session (libssh2:create-ssh-connection
                    (ip-address self)
                    :hosts-db (namestring known-hosts)
                    :port (ssh-port self))))
      (unless (authentication session (libssh2:make-publickey-auth
                                       (ssh-user self)
                                       (pathname-directory
                                        (secret-file "id_rsa"))
                                       "id_rsa"))
        (error 'libssh2::ssh-authentication-failure))
      (log:info "Initial SSH connection: ~a" session)
      session)))

(defmethod call-with-ssh-connection ((Self base-instance) fn &key (non-blocking t))
  (let ((caching-enabled-p (and non-blocking
                                (boundp '*ssh-connection-cache*))))
   (let ((conn (cond
                 (caching-enabled-p
                  (util/misc:or-setf
                   (a:assoc-value *ssh-connection-cache*
                                  self)
                   (ssh-connection self)))
                 (t
                  (ssh-connection self)))))
     (when non-blocking
       (libssh2::session-set-blocking
        (libssh2:session conn)
        :non-blocking))
     (unwind-protect
          (funcall fn conn)
       (unless caching-enabled-p
         (libssh2:destroy-ssh-Connection conn))))))

(defmacro with-ssh-connection ((conn self &key non-blocking) &body body)
  `(call-with-ssh-connection
    ,self
    (lambda (,conn) ,@body)
    :non-blocking ,non-blocking))

(defgeneric ssh-port (instance)
  (:method (instance)
    22))

(defmethod ssh-run (instance (cmd list) &rest args &key &allow-other-keys)
  (apply #'ssh-run
           instance
           (encode-bash-command cmd)
           args))

(defmethod ssh-sudo (instance (cmd list) &rest args &key &allow-other-keys)
  (apply #'ssh-sudo
         instance
         (encode-bash-command cmd)
         args))

(defmethod ssh-user (instance)
  "root")

(defun encode-bash-command (list)
  (str:join " " (mapcar #'uiop:escape-sh-token list)))

(Defun call-with-instance (fn &rest args)
  (let ((instance (apply #'create-instance args))
        (delete? t))
   (unwind-protect
        (restart-case
            (progn
              (wait-for-ready instance)
              (funcall fn instance))
          (leave-instance-as-is ()
            (setf *last-instance* instance)
            (setf delete? nil)))
     (when delete?
      (delete-instance instance)))))

(defmacro with-instance ((instance &rest create-instance-args) &body body)
  `(call-with-instance
    (lambda (,instance)
      ,@body)
    ,@create-instance-args))

(auto-restart:with-auto-restart ()
 (defun download-file (url output)
   (log:info "Downloading ~a" url)
   (let ((input (util/request:http-request
                 url
                 :ensure-success t
                 :want-stream t
                 :force-binary t)))
     (with-open-file (output output :element-type '(unsigned-byte 8)
                                     :direction :output
                                     :if-exists :supersede)
       (uiop:copy-stream-to-stream
        input
        output
        :element-type '(unsigned-byte 8))))))


(defun add-url (instance url output)
  (let ((cache (ensure-directories-exist
                (make-pathname
                 :name (ironclad:byte-array-to-hex-string (md5:md5sum-string url))
                 :defaults (asdf:system-relative-pathname
                            :screenshotbot
                            "../../build/web-cache/")))))
    (unless (path:-e cache)
      (download-file url cache))
    (assert (path:-e cache))
    (scp instance cache output)))


(defvar *test* "localhost ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMi3Pnzlr84phRm3h6eKmX4FaVtrZuQ0kUCcplbofdL1
localhost ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDacgrVkYfIr0L8dbyanWOFy+P6Ltee0NKQufoiLKnmiTeFfjQxKCl/zDXTX3B7ZjjMAcGNazLEkswYilB4XMqXtUyoBNCTtcHU/PToSJ/UJk+qXn0+ieh/Kvv6rpr+4Kks0kUvyb/5EKf0G+eoHTAaS5CvGSYiZ4FT0ReObp1unHY5Q9eycLunYsCloKiUrOQT33p9qBh0tUID0VZlUyHinKKG8LRFYbm4XautECng26SpIzMSxrU6/XFKI7+ou98P6A8S31FPLbXQ+6dQ/kwevhlgwYsQZYf0CtibGSvsLRMLFahmRzkPUfw3wmnwutEqNdpdH/NKrafO5w54up1/H35ouyrwoNCbw+cFM8qEdOwcO2wgv7RbwjRVzlxl+erIrdu58DBGvBJPDyYBedUNOi4vtwDUJtPqxgK1V8ew6WPHicox9qDm5L7nGsfgF7go3DIRRtqRQDku/mK8kmAtziSeJ63iXAUXZpFo1jW7PmUki1+mlm1cUY3EKn/Wjb8=
localhost ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIxj05X+4ZdA96pHEBYfRsgNQO2kKUBbkBnuUNcQ+aZqg7z78K1RT3vGTr9100SP2bLB3kDPID4xJf+357bp4HY=")

(defun rewrite-known-hosts (input ip-addr)
  "Rewrite the output of ssh-keyscan localhost"
  (let ((scanner (cl-ppcre:create-scanner "^localhost " :multi-line-mode t)))
   (cl-ppcre:regex-replace-all scanner
                               input
                               (format nil "~a " ip-addr))))

;; (format t (rewrite-known-hosts *test* "1.2.3.4"))


(defun secret-file (name)
  (namestring
   (path:catfile
    (asdf:system-relative-pathname
     :screenshotbot
     "../../.secrets/")
    name)))

(defmethod read-streams ((stream libssh2:ssh-channel-stream)
                         stdout-callback
                         stderr-callback
                         &key (interrupt-fn (lambda () nil)))
  (let* ((socket (libssh2:socket stream))
         (channel (libssh2:channel stream))
         (+size+ 2560)
         (arr (make-array +size+ :element-type '(unsigned-byte 8) :allocation :static)))

    (unwind-protect
         (progn
           #+lispworks
           (mp:notice-fd (usocket:socket socket))

           (loop while (not (funcall interrupt-fn)) do
             (when (libssh2:channel-eofp channel)
               (return))

             (let ((did-read? nil))
               (flet ((read-stream (stream callback)
                        (let ((read (libssh2:channel-read
                                     channel
                                     arr
                                     :end +size+
                                     :type stream
                                     :non-blocking t)))
                          (cond
                            ((eql libssh2:+eagain+ read)
                             (libssh2:wait-for-fd channel))
                            ((< read 0)
                             (error "Error while reading SSH stream"))
                            ((> read 0)
                             (setf did-read? t)
                             (funcall callback arr read))
                            ((= 0 read)
                             (return-from read-streams))))))
                 (read-stream :stdout stdout-callback)
                 #+nil ;; not sure this works :/
                 (read-stream :stderr stderr-callback))
               (unless did-read?
                 (sleep 0.1)))))

      #+lispworks
      (mp:unnotice-fd (usocket:socket socket)))))

(defclass remote-process ()
  ((thread :initarg :thread)
   (exit-status :initform nil
                :reader exit-status)
   (terminate-flag :initform nil
                   :accessor terminate-flag)))

(defmacro with-cached-ssh-connections (() &body body)
  `(call-with-cached-ssh-connections
    (lambda () ,@body)))


(defun call-with-cached-ssh-connections (fn)
  (cond
    ((boundp '*ssh-connection-cache*)
     ;; nested calls
     (funcall fn))
    (t
     (let ((*ssh-connection-cache* nil))
       (unwind-protect
            (funcall fn)
         (loop for (nil . conn) in *ssh-connection-cache*
               do
                  (libssh2:destroy-ssh-connection conn)))))))


(auto-restart:with-auto-restart ()
 (defmethod ssh-run ((self t) cmd
                     &key (output *standard-output*)
                       (error-output *standard-output*)
                       (interrupt-fn (lambda () nil)))
   (log:info "Running via core SSH: ~a" cmd)
   (with-ssh-connection (conn self :non-blocking t)
     (libssh2:with-execute (stream conn (format nil "~a 2>/dev/null" cmd))
       (labels ((flush-output (output buf size)
                  (write-sequence
                   (flexi-streams:octets-to-string buf
                                                   :end  size)
                   output))
                (do-reading (output)
                  (read-streams stream
                                (lambda (buf size)
                                  (flush-output output buf size))
                                (lambda (buf size)
                                  (flush-output error-output buf size))
                                :interrupt-fn interrupt-fn)))

         (cond
           ((eql 'string output)
            (with-output-to-string (out)
              (do-reading out)))
           (t
            (do-reading output))))))))




#+nil
(libssh2:with-ssh-connection conn
    ("tdrhq.com"
     (libssh2:make-publickey-auth
      "arnold"
      (pathname-directory #P "~/.ssh/")
      "id_rsa")
     :hosts-db (namestring #P "/home/arnold/.ssh/known_hosts")
     :port 22)
  (libssh2:with-execute (stream conn "ls")
    (let ((stream (flexi-streams:make-flexi-stream stream)))
     (format t "~a~%"
             (uiop:slurp-input-stream 'string stream)))))

(defmethod ssh-sudo ((self t) (cmd string) &rest args)
  (apply #'ssh-run
         self (format nil "sudo ~a" cmd)
         args))


(auto-restart:with-auto-restart ()
  (defmethod scp ((self t) from to)
    (log:info "Copying via core SCP")
    (with-ssh-connection (conn self :non-blocking t)
      (with-open-file (in from
                          :direction :input
                          :element-type '(unsigned-byte 8))
       (libssh2:scp-put
        from
        to
        conn)))))

(auto-restart:with-auto-restart ()
 (defmethod http-request-via ((self t)
                              url &rest args)
   (let ((uri (quri:uri url)))
    (with-ssh-connection (conn self :non-blocking t)
      (let ((stream (libssh2:channel-direct-tcpip
                     conn
                     (quri:uri-host uri)
                     (or (quri:uri-port uri) 80))))
        ;; The stream will be closed by drakma
        (let ((stream (flexi-streams:make-flexi-stream (chunga:make-chunked-stream stream))))
          (apply #'util/request:http-request
                 url
                 :stream stream
                 :close t
                 args)))))))

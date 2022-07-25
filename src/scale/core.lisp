(defpackage :scale/core
  (:use #:cl)
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
   #:ssh-config
   #:ssh-sudo
   #:ssh-user))
(in-package :scale/core)

(defvar *last-instance*)
(defgeneric create-instance (provider type &key region))

(defgeneric delete-instance (instance))

(defgeneric wait-for-ready (instance))

(defgeneric ssh-run (instance cmd &key output error-output))

(defgeneric scp (instance local-file remote-file))

(defgeneric ip-address (instance))

(defgeneric known-hosts (instance))

(defgeneric ssh-config (instance))

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
  (cl-ppcre:regex-replace-all "^localhost "
                              input
                              (format nil "~a " ip-addr)))

;; (rewrite-known-hosts *test* "1.2.3.4")

(defmacro with-ssh-config ((config name) self &body body)
  (let ((self-sym (gensym "self"))
        (s (gensym "s")))
    `(let ((,self-sym ,self))
       (uiop:with-temporary-file (:pathname ,config :stream ,s :direction :output)
         (declare (ignorable ,config))
         (write-string (ssh-config ,self-sym)
                       ,s)
         (finish-output ,s)
         (uiop:with-temporary-file (:pathname ,name :stream ,s :direction :output)
           (declare (ignorable ,name))
           (write-string (rewrite-known-hosts (known-hosts ,self-sym)
                                              (ip-address ,self-sym)) ,s)
           (finish-output ,s)
           ,@body)))))

(defun secret-file (name)
  (namestring
   (path:catfile
    (asdf:system-relative-pathname
     :screenshotbot
     "../../.secrets/")
    name)))


(auto-restart:with-auto-restart ()
 (defmethod ssh-run ((self t) cmd
                     &key (output *standard-output*)
                       (error-output *standard-output*))
   (log:info "Running via core SSH: ~a" cmd)
   (with-ssh-config (config known-hosts) self
     (uiop:run-program
      `("ssh" ,@ (ssh-opts self config known-hosts)
                 ,(format nil "~a@~a"
                          (ssh-user self)
                          (ip-address self))
                 "bash" "-c" ,(uiop:escape-sh-token cmd))
      :output output
      :error-output error-output))))

(defmethod ssh-sudo ((self t) (cmd string) &rest args)
  (apply #'ssh-run
         self (format nil "sudo ~a" cmd)
         args))

(defmethod ssh-opts ((self t) config known-hosts)
  `("-F" ,(namestring config)
         "-o" ,(format nil "UserKnownHostsFile=~a" (namestring known-hosts))
         "-i" ,(secret-file "id_rsa")))

(auto-restart:with-auto-restart ()
  (defmethod scp ((self t) from to)
    (log:info "Copying via core SCP")
    (with-ssh-config (config known-hosts) self
     (uiop:run-program
      `("scp"
        ,@ (ssh-opts self config known-hosts)
        ,(namestring from)
        ,(format nil "~a@~a:~a" (ssh-user self)
                 (ip-address self)
                 (namestring to)))
      :output *standard-output*
      :error-output *standard-output*))))

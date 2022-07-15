(defpackage :scale/linode
  (:use #:cl
        #:scale/core)
  (:local-nicknames (#:a #:alexandria)))
(in-package :scale/linode)

(defun secret-file (name)
  (namestring
   (path:catfile
    (asdf:system-relative-pathname
     :screenshotbot
     "../../.secrets/")
    name)))

(defvar *lock* (bt:make-lock))

(defvar *instances* nil)

(hunchentoot:define-easy-handler (linode-ready
                                  :uri "/scale/linode/ready")
    (secret)
  (log:info "Got callback for secret: ~a" secret)
  (let ((secret (parse-integer secret)))
   (loop for instance in *instances*
         if (eql (instance-secret instance) secret)
           do
              (unless (callback-received-p instance)
                (let ((body (hunchentoot:raw-post-data :force-text t)))
                  (log:info "got body as: ~S" body)
                  (bt:with-lock-held ((lock instance))
                    (setf (callback-received-p instance) t)
                    (setf (known-hosts instance) body)
                    (bt:condition-notify (cv instance))))))))

(defclass linode ()
  ((access-token :initarg :access-token
                 :reader access-token
                 :initform (str:trim (uiop:read-file-string
                                      (secret-file "linode-api-token"))))
   (callback-server
    :initarg :callback-server
    :initform "https://staging.screenshotbot.io"
    :reader callback-server)))

(defclass instance ()
  ((id :initarg :id
       :reader instance-id)
   (callback-received-p
    :accessor callback-received-p
    :initform nil)
   (ipv4 :initarg :ipv4
         :reader ip-address)
   (secret :initarg :secret
           :reader instance-secret)
   (known-hosts :initarg :known-hosts
                :accessor known-hosts)
   (cv :initform (bt:make-condition-variable)
       :reader cv)
   (lock :initform (bt:make-lock)
         :reader lock)
   (provider :initarg :provider
             :reader provider)))

(defun http-request (linode url &rest args &key parameters &allow-other-keys)
  (multiple-value-bind (response err)
      (apply #'util/request:http-request
               (format nil "https://api.linode.com/v4~a" url)
               :additional-headers `(("Authorization" . ,(format nil "Bearer ~a" (access-token linode))))
               :content-type "application/json"
               :content
                (if parameters
                    (json:encode-json-to-string parameters)
                    "{}")
               :want-string t
               (a:remove-from-plist args :parameters))

    (unless (= err 200)
      (error "API Request failed"))
    (let ((response (json:decode-json-from-string response)))
      response)))

(defun linode-types (linode)
  (http-request linode
                "/linode/types"))

(defun linode-images (linode)
  (http-request linode "/images"))

(defmethod callback-url ((self linode) secret)
  (quri:render-uri
   (quri:merge-uris
    (quri:uri (format nil "/scale/linode/ready?secret=~a"
                      secret))
    (quri:uri (callback-server self)))))


(defmethod create-instance ((self linode)
                            type
                            &key                              region)
  (declare (ignore type region))
  (let ((secret (secure-random:number 1000000000000000000000000000000000000)))
   (let ((image (http-request
                 self
                 "/linode/instances"
                 :method :post
                 :parameters `(("image" . "linode/debian11")
                               ("root_pass" . "ArnoshLighthouse1987")
                               ("authorized_keys" .
                                                  #(,(str:trim (uiop:read-file-string (secret-file "id_rsa.pub")))))
                               ("region" . "us-east")
                               ("tags" . #("scale"))
                               ("type" . "g6-nanode-1")
                               ("stackscript_id" . 1024979)
                               ("stackscript_data"
                                . (("SB_CALLBACK_URL" . ,(callback-url self secret))))))))
     (let ((instance
             (make-instance 'instance
                             :id (a:assoc-value image :id)
                             :provider self
                             :secret secret
                             :ipv4 (car (a:assoc-value image :ipv-4)))))
       (bt:with-lock-held (*lock*)
        (push instance *instances*))
       instance))))

(defmethod delete-instance ((instance instance))
  (log:info "Deleting ~a" instance)
  (bt:with-lock-held (*lock*)
   (a:removef *instances* instance))
  (http-request
   (provider instance)
   (format nil "/linode/instances/~a" (instance-id instance))
   :method :delete))

(defmethod readyp ((instance instance))
  (let ((image (http-request
                (provider instance)
                (format nil "/linode/instances/~a" (instance-id instance)))))
    #+nil
    (log:info "Got response: ~S" image)
    (let ((status (a:assoc-value image :status)))
      (log:info "Instance not ready: ~a" status)
      (equal "running" status))))

(defmethod wait-for-ready ((instance instance))
  (bt:with-lock-held ((lock instance))
    (loop for i from 0 to 100
          do
             (cond
               ((callback-received-p instance)
                (return))
               (t
                (log:info "Waiting for instance ~a to be ready" instance)
                (bt:condition-wait (cv instance) (lock instance)
                                   :timeout 1)))
          finally
          (error "Did not receive callback"))))

(defvar *test* "localhost ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMi3Pnzlr84phRm3h6eKmX4FaVtrZuQ0kUCcplbofdL1
localhost ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDacgrVkYfIr0L8dbyanWOFy+P6Ltee0NKQufoiLKnmiTeFfjQxKCl/zDXTX3B7ZjjMAcGNazLEkswYilB4XMqXtUyoBNCTtcHU/PToSJ/UJk+qXn0+ieh/Kvv6rpr+4Kks0kUvyb/5EKf0G+eoHTAaS5CvGSYiZ4FT0ReObp1unHY5Q9eycLunYsCloKiUrOQT33p9qBh0tUID0VZlUyHinKKG8LRFYbm4XautECng26SpIzMSxrU6/XFKI7+ou98P6A8S31FPLbXQ+6dQ/kwevhlgwYsQZYf0CtibGSvsLRMLFahmRzkPUfw3wmnwutEqNdpdH/NKrafO5w54up1/H35ouyrwoNCbw+cFM8qEdOwcO2wgv7RbwjRVzlxl+erIrdu58DBGvBJPDyYBedUNOi4vtwDUJtPqxgK1V8ew6WPHicox9qDm5L7nGsfgF7go3DIRRtqRQDku/mK8kmAtziSeJ63iXAUXZpFo1jW7PmUki1+mlm1cUY3EKn/Wjb8=
localhost ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIxj05X+4ZdA96pHEBYfRsgNQO2kKUBbkBnuUNcQ+aZqg7z78K1RT3vGTr9100SP2bLB3kDPID4xJf+357bp4HY=")

(defun rewrite-known-hosts (input ip-addr)
  (cl-ppcre:regex-replace-all "^localhost "
                              input
                              (format nil "~a " ip-addr)))

;; (rewrite-known-hosts *test* "1.2.3.4")

(auto-restart:with-auto-restart ()
 (defmethod ssh-run ((self instance) cmd
                     &key (output *standard-output*)
                       (error-output *standard-output*))
   (uiop:with-temporary-file (:pathname known-hosts :stream s :direction :output)
     (write-string (rewrite-known-hosts (known-hosts self)
                                        (ip-address self)) s)
     (finish-output s)
     (uiop:run-program
      `("ssh" "-o" ,(format nil "UserKnownHostsFile=~a" (namestring known-hosts))
              "-i" ,(secret-file "id_rsa")
              ,(format nil "root@~a" (ip-address self))
              "bash" "-c" ,(uiop:escape-sh-token cmd))
      :output output
      :error-output error-output))))

#+nil
(let ((linode (make-instance 'linode :callback-server "https://staging.screenshotbot.io")))
  (let ((instance (create-instance linode :small)))
    (unwind-protect
         (progn
           (wait-for-ready instance)
           (ssh-run instance "ls -a  /root/"))
      (delete-instance instance))))

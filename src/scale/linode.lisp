(defpackage :scale/linode
  (:use #:cl
        #:scale/core)
  (:local-nicknames (#:a #:alexandria)))
(in-package :scale/linode)

(defclass linode ()
  ((access-token :initarg :access-token
                 :reader access-token
                 :initform (str:trim (uiop:read-file-string "~/.linode-api-token")))))

(defclass instance ()
  ((id :initarg :id
       :reader instance-id)
   (ipv4 :initarg :ipv4
         :reader ip-address)
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


(defmethod create-instance ((self linode)
                            type
                            &key                              region)
  (declare (ignore type region))
  (let ((image (http-request
                self
                "/linode/instances"
                :method :post
                :parameters `(("image" . "linode/debian11")
                              ("root_pass" . "ArnoshLighthouse1987")
                              ("region" . "us-east")
                              ("tags" . #("scale"))
                              ("type" . "g6-nanode-1")
                              ("stackscript_id" . 1024979)
                              ("stackscript_data"
                               . (("SB_AUTHORIZED_KEYS"
                                   . ,(uiop:read-file-string "~/.ssh/id_rsa.pub"))
                                  ("SB_SECRET" . "secret")))))))
    (make-instance 'instance
                    :id (a:assoc-value image :id)
                    :provider self
                    :ipv4 (car (a:assoc-value image :ipv-4)))))

(defmethod delete-instance ((instance instance))
  (http-request
   (provider instance)
   (format nil "/linode/instances/~a" (instance-id instance))
   :method :delete))

(auto-restart:with-auto-restart ()
 (defmethod ssh-run ((self instance) cmd
                     &key (output *standard-output*)
                       (error-output *standard-output*))
   (uiop:run-program
    `("ssh" "-o" "StrictHostKeyChecking=no"
            ,(format nil "root@~a" (ip-address self))
            "bash" "-c" ,(uiop:escape-sh-token cmd))
    :output output
    :error-output error-output)))

#+nil
(let ((linode (make-instance 'linode)))
  (let ((instance (create-instance linode :small)))
    (unwind-protect
         (progn
           (ssh-run instance "ls /root/"))
      (delete-instance instance))))

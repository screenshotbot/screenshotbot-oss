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
   (ipv4 :initarg :ipv4)))

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
                              ("type" . "g6-nanode-1")))))
    (make-instance 'instance
                    :id (a:assoc-value image :id)
                    :ipv4 (a:assoc-value image :ipv-4))))

(defmethod delete-instance ((self linode)
                            (instance instance))
  (http-request
   self
   (format nil "/linode/instances/~a" (instance-id instance))
   :method :delete))


#+nil
(let ((linode (make-instance 'linode)))
  (create-instance linode :small))

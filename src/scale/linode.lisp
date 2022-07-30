(defpackage :scale/linode
  (:use #:cl
        #:scale/core)
  (:import-from #:scale/core
                #:snapshot-exists-p)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:*linode*))
(in-package :scale/linode)

(defvar *lock* (bt:make-lock))

(defvar *instances* nil)

(defclass image (store-object)
  ((image-id :initarg :image-id
             :reader image-id)
   (known-hosts :initarg :known-hosts
                :initform nil
                :reader known-hosts)
   (snapshot-name :initarg :snapshot-name
                  :reader snapshot-name
                  :index-type hash-index
                  :index-initargs (:test #'equal)
                  :index-reader images-by-snapshot-name))
  (:metaclass persistent-class))

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
                 :accessor access-token
                 :initform (str:trim (uiop:read-file-string
                                      (secret-file "linode-api-token"))))
   (callback-server
    :initarg :callback-server
    :initform "https://staging.screenshotbot.io"
    :reader callback-server)))

(defvar *linode* (make-instance 'linode))

(defclass instance (base-instance)
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

(define-condition not-found (error)
  ((url :initarg :url)))

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

    (when (= err 404)
      (error 'not-found :url url))

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


(defmethod fix-image-name ((self linode) snapshot-name)
  (cond
    ((str:starts-with-p "linode/" snapshot-name)
     snapshot-name)
    (t
     (let ((image (snapshot-exists-p self snapshot-name)))
       (assert image)
       (image-id image)))))

(defmethod create-instance ((self linode)
                            type
                            &key region
                              (image "linode/debian11") &allow-other-keys)
  (declare (ignore type region))
  (let ((secret (secure-random:number 1000000000000000000000000000000000000)))
    (log:info "Making linode create-instance request")
    (let ((ret (http-request
                self
                "/linode/instances"
                :method :post
                :parameters `(("image" . ,(fix-image-name self image))
                              ("root_pass" . ,(format nil "a~a" (secure-random:number 100000000000000)))
                              ("authorized_keys" .
                                                 #(,(str:trim (uiop:read-file-string (secret-file "id_rsa.pub")))))
                              ("region" . "us-east")
                              ("tags" . #("scale"))
                              ("type" . "g6-nanode-1")

                              ;; Only use the stackscript if we're using a base image
                              ,@ (when (str:starts-with-p "linode/" image)
                                   `(("stackscript_id" . 1024979)
                                     ("stackscript_data"
                                      . (("SB_CALLBACK_URL" . ,(callback-url self secret))))))))))


      (let ((instance
              (make-instance 'instance
                              :id (a:assoc-value ret :id)
                              :provider self
                              :secret secret
                              :ipv4 (car (a:assoc-value ret :ipv-4)))))

        (bt:with-lock-held (*lock*)
          (push instance *instances*))

        (cond
          ((not (str:starts-with-p "linode/" image))
           ;; We don't expect to get a callback, let's manually fix the
           ;; known-hosts from the image.
           (let ((image (find-snapshot self image)))
             (assert image)
             (setf (known-hosts instance) (known-hosts image)
                   (callback-received-p instance) t)
             (loop for i from 0 to 300
                   if (readyp instance)
                     do
                        (return)
                   else
                     do
                        (log:info "Waiting for imaged instance to be ready")
                        (sleep 1)
                   finally
                   (error "image wasn't ready in time"))))
          (t
           (wait-for-ready instance)))

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


#+nil
(with-instance (self (make-instance 'linode) :small)
  (scp self "/home/arnold/builds/web/.gitignore" "/root/default.css")
  (ssh-run self "ls /root/"))

(defun image-end-point (snapshot-name)
  (format nil "/images/private/~a" snapshot-name))

(defmethod find-snapshot ((self linode) snapshot-name)
  (let ((images (images-by-snapshot-name snapshot-name)))
    (log:info "Got images: ~s for ~a" images snapshot-name)
    (loop for image in images
          for api-response = (handler-case
                                 (progn
                                   (http-request
                                    self
                                    (format nil "/images/~a" (image-id image))))
                               (not-found ()
                                 (log:warn "An image object existed locally  but could not find the image on Linode")
                                 nil))
          if api-response
            return (values image api-response))))

(auto-restart:with-auto-restart ()
  (defmethod snapshot-exists-p ((self linode) snapshot-name)
    (multiple-value-bind (image response)
        (find-snapshot self snapshot-name)
      (when (and image (string-equal "available" (a:assoc-value response :status)))
        image))))

(defmethod snapshot-pending-p ((self linode) snapshot-name)
  (multiple-value-bind (image response)
      (find-snapshot self snapshot-name)
    (and
     image
     (not (string-equal "available" (a:assoc-value response :status))))))


(defmethod get-disk-id ((self instance))
  (let ((response (http-request
                   (provider self)
                   (format nil "/linode/instances/~a/disks" (instance-id self)))))
    (loop for disk in (a:assoc-value response :data)
          if (string-equal "ext4" (a:assoc-value disk :filesystem))
            return (a:assoc-value disk :id))))

(auto-restart:with-auto-restart ()
  (defmethod make-snapshot ((self instance) snapshot-name)
    (let ((label (a:lastcar (str:split "-" snapshot-name))))
     (let ((disk-id (get-disk-id self)))
       (let ((response (http-request
                        (provider self)
                        "/images"
                        :method :post
                        :parameters `(("description" . "Automatically created snapshot")
                                      ("label" . ,label)
                                      ("disk_id" . ,disk-id)))))
         (let ((known-hosts (ssh-run self "ssh-keyscan localhost")))
           (make-instance 'image
                          :snapshot-name snapshot-name
                          :known-hosts known-hosts
                          :image-id (a:assoc-value response :id))))))))

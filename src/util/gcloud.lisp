(defpackage :util/gcloud
  (:use #:cl)
  (:shadow #:machine-type))
(in-package :util/gcloud)

(defclass gcloud-vm-provider ()
  ((zone :initarg :zone
         :initform "us-east4-c"
         :reader zone)
   (machine-type :initarg :machine-type
                 :initform "n2-highcpu-8"
                 :reader machine-type)
   (prefix :initarg :prefix
           :initform "auto-vm"
           :reader prefix)
   (project :initarg :project
            :reader project)
   (metadata :initarg :metadata
             :reader metadata)
   (source-machine-imagoe :initarg :source-machine-image
                         :reader source-machine-image)))

(defclass gcloud-vm ()
  ((provider :initarg :provider
             :reader gcloud-vm-provider)
   (vm-name :initarg :vm-name
            :reader vm-name)))

(defmethod start-instance ((self gcloud-vm-provider))
  (let* ((id (mongoid:oid-str (mongoid:oid)))
         (vm-name (str:downcase (format nil "~a-~a" (prefix self) id))))
    (uiop:run-program
     (remove-if
      #'null
      (list*
       "gcloud" "compute" "instances" "create"
       vm-name
       "--zone" (zone self)
       "--min-cpu-platform" "Intel Cascadelake"
       (when (metadata self)
         "--metadata")
       (when (metadata self)
         (str:join
          ","
          (loop for (key . value) in (metadata self)
                collect (format nil "~a=~a" key value))))
       "--machine-type"
       (machine-type self)
       (image-args self)))
     :error-output *standard-output*
     :output *standard-output*)
    (make-instance 'gcloud-vm
                   :provider self
                   :vm-name vm-name )))

(defmethod stop-instance ((self gcloud-vm))
  (uiop:run-program
   (list
    "gcloud" "compute" "instances" "delete"
    (vm-name self)
    "--zone" (zone (gcloud-vm-provider self))
    "-q")
   :output *standard-output*
   :error-output *standard-output*))

;; (time (setf *vm* (start-instance *provider*)))
;; (stop-instance *vm*)


(defmethod image-args ((Self gcloud-vm-provider))
  (list
   "--source-machine-image"
   (format
    nil
    "https://www.googleapis.com/compute/v1/projects/~a/global/machineImages/~a"
    (project self)
    (source-machine-image self))))

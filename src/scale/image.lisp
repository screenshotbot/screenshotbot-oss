(defpackage :scale/image
  (:use #:cl)
  (:import-from #:scale/core
                #:with-cached-ssh-connections
                #:delete-instance
                #:make-snapshot
                #:create-instance-from-snapshot
                #:snapshot-exists-p
                #:create-instance
                #:with-instance)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:create-image-instance
   #:image-spec-serialized-key))
(in-package :scale/image)

(defclass image-spec ()
  ((init-fn :initarg :init-fn
            :reader init-fn)
   (name :initarg :name)
   (version :initarg :version
            :reader image-spec-version)
   (parent-image-fn :initarg :parent-image-fn
                    :reader parent-image-fn)
   (parent-image)))

(defvar *image-specs* nil)

(defvar *root-specs* (list :debian-11))

(defmacro defimage ((name &key (instance (gensym "instance"))
                            (version 0))
                    parameters
                    parent-image-spec
                    &body body)
  (let ((parent-image-spec (if (symbolp parent-image-spec)
                               (list parent-image-spec)
                               parent-image-spec)))
   `(progn
      (setf
       (a:assoc-value
        *image-specs*
        ',name)
       (make-instance 'image-spec
                      :name ',name
                      :version 'version
                      :init-fn (lambda (,instance &key ,@parameters)
                                 (declare (ignorable instance))
                                 ,@body)
                      :parent-image-fn (lambda (&key ,@parameters)
                                         (declare (ignorable ,@parameters))
                                         ;; todo: more carefully...
                                          (list
                                           ',(car parent-image-spec)
                                           ,@ (cdr parent-image-spec))))))))

(defun root-spec-p (spec)
  (cond
    ((and
      (listp spec)
      (> (length spec) 1))
     nil)
    ((symbolp spec)
     (member spec *root-specs*))
    (t
     (root-spec-p (car spec)))))

(defgeneric image-spec-key (image-spec))

(defmethod image-spec-key ((image-spec t))
  (let ((image-spec (if (symbolp image-spec) (list image-spec) image-spec)))
    (destructuring-bind (name . args) image-spec
      (case name
        (:debian-11
         :debian-11)
        (otherwise
         (let ((spec (a:assoc-value *image-specs* name)))
           (unless spec
             (error "could not find spec for ~a" name))
           (let ((parent (apply (parent-image-fn spec)  args)))
             `(:name ,name
               :version ,(image-spec-version spec)
               :parent ,(image-spec-key parent)))))))))

(defmethod image-spec-serialized-key (image-spec)
  (let ((spec-key (image-spec-key image-spec)))
    (let ((*package* (find-package "CL-USER")))
      (str:replace-all ":" "-"
       (str:replace-all "/" "-"
                        (format nil "~s-~a"
                                (if (symbolp image-spec) image-spec (car image-spec))
                                (ironclad:byte-array-to-hex-string
                                 (md5:md5sum-string
                                  (with-output-to-string (out)
                                    (format out "~S" spec-key))))))))))

(defmethod apply-image (instance (image-spec (eql :debian-11))
                        &key apply-parent))

(defmethod apply-image (instance image-spec &key (apply-parent t))
  (when (not (root-spec-p image-spec))
   (let ((image-spec (if (symbolp image-spec) (list image-spec) image-spec)))
     (destructuring-bind (name . args) image-spec
       (let ((spec (a:assoc-value *image-specs* name)))
         (unless spec
           (error "Could not find spec for ~a" name))
         (when apply-parent
          (let ((parent (apply (parent-image-fn spec) args)))
            (apply-image instance parent)))
         (apply (init-fn spec) instance args))))))

(defmacro with-imaged-instance ((instance image provider &key size) &body body)
  `(call-with-imaged-instance
    (lambda (,instance)
      ,@body)
    ',image
    ,provider
    :size ,size))

(defun call-with-imaged-instance (fn image provider &key size)
  (let ((instance (create-image-instance image provider :size size)))
    (with-cached-ssh-connections ()
     (unwind-protect
          (funcall fn instance)
       (delete-instance instance)))))

(defun create-image-instance (image provider &key size)
  "Creates an instance, but make sure you call destroy-instance on the response"
  (let ((snapshot-name (image-spec-serialized-key image)))
    (cond
      ((snapshot-exists-p provider snapshot-name)
       (create-instance provider size
                        :image snapshot-name))
      (t
       (with-instance (instance provider size)
         ;; replay the image on it for now.. in the future, we'll actually
         ;; cache the image
         (scale/core:with-cached-ssh-connections ()
           (apply-image instance image)
           (make-snapshot instance snapshot-name)
           instance))))))

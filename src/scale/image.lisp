(defpackage :scale/image
  (:use #:cl)
  (:import-from #:scale/core
                #:snapshot-pending-p
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
                      :version ,version
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

(defun destructure-spec (image-spec)
  (when (not (root-spec-p image-spec))
    (let ((image-spec (if (symbolp image-spec) (list image-spec) image-spec)))
      (destructuring-bind (name . args) image-spec
        (let ((spec (a:assoc-value *image-specs* name)))
          (unless spec
            (error "Could not find spec for ~a" name))
          (values
           spec
           args
           (apply (parent-image-fn spec) args)))))))

(defmethod apply-image (instance image-spec &key (apply-parent t))
  (multiple-value-bind (spec args parent) (destructure-spec image-spec)
    (when apply-parent
      (apply-image instance parent))
    (apply (init-fn spec) instance args)))

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
  (cond
    ((or (eql :debian-11 image)
         (equal '(:debian-11) image))
     (create-instance provider :debian-11 :size size))
    (t
     (let ((snapshot-name (image-spec-serialized-key image)))
       (cond
         ((snapshot-exists-p provider snapshot-name)
          (create-instance provider size
                           :image snapshot-name))
         (t

          (multiple-value-bind (spec args parent-spec) (destructure-spec image)
            (declare (ignore spec args))
            (let ((instance (create-image-instance parent-spec
                                                   provider
                                                   :size size)))
              (scale/core:with-cached-ssh-connections ()
                (apply-image instance image :apply-parent nil))
              (unless (snapshot-pending-p provider snapshot-name)
                (log:info "No snapshot pending, making snapshot")
                (make-snapshot instance snapshot-name))
              instance))))))))

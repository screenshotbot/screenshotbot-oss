(defpackage :scale/image
  (:use #:cl)
  (:import-from #:scale/core
                #:create-instance
                #:with-instance)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:create-image-instance))
(in-package :scale/image)

(defclass image-spec ()
  ((init-fn :initarg :init-fn
            :reader init-fn)
   (parent-image-fn :initarg :parent-image-fn
                    :reader parent-image-fn)
   (parent-image)))

(defvar *image-specs* nil)

(defvar *root-specs* (list :debian-11))

(defmacro defimage ((name &key (instance (gensym "instance")))
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

(defmethod apply-image (instance (image-spec (eql :debian-11))))

(defmethod apply-image (instance image-spec)
  (when (not (root-spec-p image-spec))
   (let ((image-spec (if (symbolp image-spec) (list image-spec) image-spec)))
     (destructuring-bind (name . args) image-spec
       (let ((spec (a:assoc-value *image-specs* name)))
         (unless spec
           (error "Could not find spec for ~a" name))
         (let ((parent (apply (parent-image-fn spec) args)))
           (apply-image instance parent))
         (apply (init-fn spec) instance args))))))

(defmacro with-imaged-instance ((instance image provider &key size) &body body)
  `(with-instance (,instance ,provider ,size)
     ;; replay the image on it for now.. in the future, we'll actually
     ;; cache the image
     (apply-image ,instance ',image)
     ,@body))

(defun create-image-instance (image provider &key size)
  "Creates an instance, but make sure you call destroy-instance on the response"
  (let ((instance (create-instance provider size)))
    (apply-image instance image)
    instance))

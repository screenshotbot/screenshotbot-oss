(defpackage :pixel-diff/image-pair
  (:use #:cl)
  (:export #:image-pair
           #:previous
           #:updated
           #:make-image-pair))
(in-package :pixel-diff/image-pair)

(defclass image-pair ()
  ((previous :initarg :previous
             :accessor previous
             :type (or string pathname null)
             :documentation "Image-layer of the previous file")
   (updated :initarg :updated
            :accessor updated
            :type (or string pathname null)
            :documentation "Image-layer of the next file"))
  (:documentation "Represents a pair of image files for comparison"))

(defun make-image-pair (previous updated)
  "Create a new image-pair instance with the given previous and updated image paths."
  (make-instance 'image-pair :previous previous :updated updated))



(defpackage :pixel-diff/external-images
  (:use #:cl
        #:capi)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:with-image-from-external))
(in-package :pixel-diff/external-images)

(defvar *clock* 0)

(defclass cache-entry ()
  ((pane :initarg :pane
         :reader pane)
   (external-image :initarg :external-image
                   :reader external-image)
   (image :initarg :image
          :reader image)
   (timestamp :initform *clock*
              :accessor timestamp)))

(defvar *cache* nil)
(defvar *max-cache-size* 5)

(defun maybe-evict-cache ()
  (when (>= (length *cache*) *max-cache-size*)
    (let ((oldest-entry (reduce (lambda (a b)
                                  (if (< (timestamp a) (timestamp b))
                                      a
                                      b))
                                *cache*)))
      (gp:free-image
       (pane oldest-entry)
       (image oldest-entry))
      (setf *cache* (remove oldest-entry *cache*)))))

(defun find-cache-entry (pane external-image)
  (find-if (lambda (entry)
             (and (eq (pane entry) pane)
                  (equal (external-image entry) external-image)))
           *cache*))


(def-easy-macro with-image-from-external (&binding image pane external-image &fn fn)
  (let ((existing (find-cache-entry pane external-image)))
    (cond
      (existing
       (log:debug "using cached image")
       (fn (image existing)))
      (t
       (log:debug "Loading fresh image")
       (let ((image (gp:load-image pane external-image :editable t)))
         (maybe-evict-cache)
         (push
          (make-instance 'cache-entry
                         :pane pane
                         :external-image external-image
                         :image image)
          *cache*)
         (fn image))))))

(defpackage :screenshotbot/model/image-comparison
  (:use #:cl)
  (:shadow #:find)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/model/transient-object
                #:make-transient-clone
                #:with-transient-copy)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:screenshotbot/model/image
                #:mask=
                #:image-filesystem-pathname
                #:draw-masks-in-place
                #:with-local-image
                #:make-image
                #:image)
  (:import-from #:screenshotbot/magick/magick
                #:run-magick)
  (:import-from #:auto-restart
                #:with-auto-restart)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:util/object-id
                #:oid
                #:oid-array)
  (:import-from #:util/store
                #:location-for-oid)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:bknr.datastore
                #:store-objects-with-class)
  (:import-from #:util/cron
                #:def-cron)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:image-comparison
   #:%image-comparisons-for-before
   #:image-comparison-after
   #:image-comparison-masks
   #:image-comparison-result
   #:identical-p))
(in-package :screenshotbot/model/image-comparison)

(defvar *lock* (bt:make-lock "image-comparison"))

(with-transient-copy (transient-image-comparison abstract-image-comparison)
  (defclass image-comparison (store-object)
    ((before :initarg :before
             :reader image-comparison-before
             :index-type hash-index
             :index-reader %image-comparisons-for-before
             :relaxed-object-reference t)
     (after :initarg :after
            :reader image-comparison-after
            :relaxed-object-reference t)
     (masks :initarg :masks
            :reader image-comparison-masks)
     (identical-p :initform nil
                  :accessor identical-p
                  :initarg :identical-p
                  :documentation "A result inducating that the images differ only in exif data")
     (result :initarg :result
             :accessor image-comparison-result))
    (:metaclass persistent-class)))

(defun transient-objects-for-before (before)
  (let ((file (location-for-before-image before)))
    (when (path:-e file)
      (cl-store:restore file))))

(defmethod make-transient-clone ((self image-comparison))
  (make-instance
   'transient-image-comparison
    :before (make-transient-clone (image-comparison-before self))
    :after (make-transient-clone (image-comparison-after self))
    :masks (mapcar #'make-transient-clone (image-comparison-masks self))
    :identical-p (identical-p self)
    :result (make-transient-clone (image-comparison-result self))))

(defmethod location-for-before-image ((self image))
  (location-for-oid #P "cl-store/image-comparisons/"
                    (oid-array self)))

(defmethod make-transient ((self image-comparison))
  "Persist the image comparison to disk, and delete the bknr object"
  (let ((old (transient-objects-for-before (image-comparison-before self))))
    (let ((filename (location-for-before-image (image-comparison-before self))))
      (uiop:with-staging-pathname (filename)
        (cl-store:store (list* (make-transient-clone self)
                               old)
                        filename))
      (assert (path:-e (location-for-before-image (image-comparison-before self))))
      (log:info "Deleting: ~s" self)
      (bknr.datastore:delete-object self))))

;; (make-transient (bknr.datastore:store-object-with-id 701037))


(defun find-old-image-comparisons ()
  "Finds old BKNR objects that haven't been touched in a while. It uses
 the timestamp of te result image to determine if they have been
 created a while ago, so it's not 100% accurate, but good enough."
  (let ((threshold (- (get-universal-time) (* 2 24 30 3600))))
   (loop for ic in (store-objects-with-class 'image-comparison)
         for result = (image-comparison-result ic)
         if (let ((file (image-filesystem-pathname result)))
              (assert (path:-e file))
              (< (file-write-date file) threshold))
           collect ic)))

(defun make-old-transient ()
  (mapc #'make-transient (find-old-image-comparisons)))

(def-cron make-old-transient (:minute 45 :hour 22)
  (make-old-transient))

(defun do-image-comparison (before-image
                            after-image
                            p
                            masks)
  "Compares before-screenshot and after-screenshot, and saves the result image to P.

If the images are identical, we return t, else we return NIL."
  (with-local-image (before before-image)
    (with-local-image (after after-image)
      (let ((cmd (list
                  "compare"
                  "-metric" "RMSE"
                  (namestring before)
                  (namestring after)
                  "-highlight-color" "red"
                  "-lowlight-color" "none"
                  "-compose" "src"
                  (namestring p))))
        (multiple-value-bind (out err ret)
            (run-magick cmd
                        :ignore-error-status t
                        :output 'string
                        :error-output 'string)

          (unless (member ret '(0 1))
            (error "Got surprising error output from imagemagic compare: ~S~%args:~%~S~%stderr:~%~a~%stdout:~%~a" ret cmd err out))
          (draw-masks-in-place p masks :color "rgba(255, 255, 0, 0.8)")
          (log:info "Got return code: ~a with output `~a`, `~a`" ret out err)
          (when (= ret 0)
            (equal "0 (0)" (str:trim err))

            ;; Slow version if we ever find that imagemagick's output
            ;; is unreliable:
            #+nil
            (block check
              (map-unequal-pixels
               before-screenshot after-screenshot
               (lambda (x y)
                 (declare (ignore x y))
                 (return-from check nil)))
              t)))))))

(defun find-existing-image-comparison (before after masks)
  (flet ((find-in (list)
          (loop for comparison in list
                if (and (string= (oid after) (oid (image-comparison-after comparison)))
                        (equal (length masks) (length (image-comparison-masks comparison)))
                        (every #'identity
                               (loop for x in masks
                                     for y in (image-comparison-masks comparison)
                                     collect (mask= x y))))
                  return comparison)))
    (or
     (find-in (%image-comparisons-for-before before))
     (let ((transient-image-comparisons (transient-objects-for-before before)))
       (find-in transient-image-comparisons)))))

(defmethod find-image-comparison-on-images ((before image)
                                            (after image)
                                            masks)
  "Finds an existing image comparison for before and after, if it
  doesn't exist calls creator with a temporary file. The creator
  should create the image in the file provided. The creator should
  returns true if the images are completely identical, or nil
  otherwise"
  (flet ((find ()
           (find-existing-image-comparison before after masks)))
    (or
     (bt:with-lock-held (*lock*)
       (find))
     (uiop:with-temporary-file (:pathname p :type "png" :prefix "comparison")
       (let ((identical-p (do-image-comparison
                            before
                            after
                            p
                            masks)))
         (let* ((image (make-image :pathname p
                                   :content-type "image/png")))
           (bt:with-lock-held (*lock*)
             (or
              (find)
              (progn
                (log:info "making new image-comparison")
                (make-instance 'image-comparison
                               :before before
                               :after after
                               :masks masks
                               :identical-p identical-p
                               :result image))))))))))

(with-auto-restart ()
  (defmethod recreate-image-comparison ((self image-comparison))
    (log:info "recreating: ~a" (bknr.datastore:store-object-id self))
    (let* ((image (image-comparison-result self))
           (pathname (image-filesystem-pathname image)))
      (check-type image image)
      (restart-case
          (progn
            (let ((before (image-comparison-before self))
                  (after (image-comparison-after self))
                  (masks (image-comparison-masks self)))
              (delete-object self)
              (find-image-comparison-on-images
               before after masks))
            (delete-object image)
            (log:info "Deleting ~a" pathname)
            (delete-file pathname))
        (ignore-this-comparison ()
          (values))))))


(defmethod recreate-all-image-comparisons ()
  (loop for image-comparison in (reverse
                                 (bknr.datastore:store-objects-with-class 'image-comparison))
        do
        (recreate-image-comparison image-comparison)))

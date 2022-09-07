(defpackage :screenshotbot/model/image-comparison
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/model/transient-object
                #:with-transient-copy)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:screenshotbot/model/image
                #:draw-masks-in-place
                #:with-local-image
                #:make-image
                #:image)
  (:import-from #:screenshotbot/magick/magick
                #:run-magick)
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


(defmethod find-image-comparison-on-images ((before image)
                                            (after image)
                                            masks)
  "Finds an existing image comparison for before and after, if it
  doesn't exist calls creator with a temporary file. The creator
  should create the image in the file provided. The creator should
  returns true if the images are completely identical, or nil
  otherwise"
  (flet ((find ()
           (loop for comparison in (%image-comparisons-for-before before)
                 if (and (eql after (image-comparison-after comparison))
                         (equal masks (image-comparison-masks comparison)))
                   return comparison)))
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
              (make-instance 'image-comparison
                              :before before
                              :after after
                              :masks masks
                              :identical-p identical-p
                              :result image)))))))))

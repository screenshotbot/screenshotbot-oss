;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/image-comparer
  (:use #:cl)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/model/image
                #:dimension=
                #:image-dimensions
                #:with-local-image
                #:image=
                #:base-image-comparer)
  (:import-from #:screenshotbot/magick/magick-lw
                #:get-non-alpha-pixels
                #:magick-get-image-width
                #:magick-get-image-height
                #:with-wand
                #:with-image-comparison)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/async
                #:magick-future)
  (:import-from #:lparallel
                #:future
                #:force)
  (:local-nicknames (#:recorder-run #:screenshotbot/model/recorder-run))
  (:export
   #:make-image-comparer))
(in-package :screenshotbot/model/image-comparer)

(defclass threshold-comparer (base-image-comparer)
  ((threshold :initarg :threshold
              :reader compare-threshold)))

(defmethod make-image-comparer (run)
  (let ((threshold (recorder-run:compare-threshold run)))
    (cond
      ((and threshold
            (numberp threshold)
            (> threshold 0))
       (make-instance 'threshold-comparer
                      :threshold threshold))
      (t
       (make-instance 'base-image-comparer)))))


(defmethod image= ((self threshold-comparer)
                   image1
                   image2
                   masks)
  (or
   (call-next-method)

   (and
    (dimension=
     (image-dimensions image1)
     (image-dimensions image2))
    (compare-with-threshold
     self
     image1
     image2
     masks))))

(defmethod compare-with-threshold ((self threshold-comparer)
                                   image1
                                   image2
                                   masks)
  (with-local-image (file1 image1)
    (with-local-image (file2 image2)
      (force
       (magick-future ()
         (with-wand (before :file file1)
           (let ((limit (floor (* (min 1.0 (compare-threshold self))
                                  (magick-get-image-height before)
                                  (magick-get-image-width before)))))
             (with-wand (after :file file2)
               (with-image-comparison (before after
                                       :result result
                                       :in-place-p t)
                 (let ((bad-pixels (get-non-alpha-pixels
                                    result
                                    ;; Why +2 instead of +1? I think it might
                                    ;; be a bug somewhere in
                                    ;; get-non-alpha-pixels, but +2 works for
                                    ;; now.
                                    :limit (+ 2 limit)
                                    :masks masks)))
                   (let ((bad-pixel-count (first (array-dimensions bad-pixels))))
                     (<= bad-pixel-count limit))))))))))))

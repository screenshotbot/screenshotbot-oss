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
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:util/store/store
                #:with-class-validation
                #:defindex)
  (:import-from #:bknr.indices
                #:unique-index)
  (:local-nicknames (#:recorder-run #:screenshotbot/model/recorder-run))
  (:export
   #:make-image-comparer))
(in-package :screenshotbot/model/image-comparer)

(defclass cached-comparer ()
  ())

(defclass threshold-comparer (cached-comparer
                              base-image-comparer)
  ((threshold :initarg :threshold
              :reader compare-threshold)
   (tolerance :initarg :tolerance
              :initform 0
              :reader compare-tolerance)))

(defmethod make-image-comparer (run)
  (flet ((gt-0 (x)
           "Check if x is greater than zero, and is a number."
           (and
            (numberp x)
            (> x 0))))
   (let ((threshold (recorder-run:compare-threshold run))
         (tolerance (recorder-run:compare-tolerance run)))
     (cond
       ((or
         (gt-0 threshold)
         (gt-0 tolerance))
        (make-instance 'threshold-comparer
                       :threshold (or threshold 0)
                       :tolerance (or tolerance 0)))
       (t
        (make-instance 'base-image-comparer))))))


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
               (check-type (compare-tolerance self) number)
               (with-image-comparison (before after
                                       :result result
                                       :pixel-tolerance (compare-tolerance self)
                                       :in-place-p t)
                 ;; TODO(T844): factor in tolerance
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

(defindex *image-equal-cache*
  'unique-index
  :test #'equal
  :slot-name '%key)


(with-class-validation
 (defclass image-equal-cache (store-object)
   ((%key :initarg :key
          :index *image-equal-cache*
          :index-reader image-equal-cache-for-key)
    (%result :initarg :result
             :reader image-equal-cache-result))
   (:metaclass persistent-class)))


(defvar *lock* (bt:make-lock))

(defmethod image= :around ((self cached-comparer)
                           image1
                           image2
                           masks)
  (let* ((key (list
               (type-of self)
               (compare-threshold self)
               (ignore-errors (compare-tolerance self))
               image1
               image2
               masks))
         (cache (image-equal-cache-for-key key)))
    (cond
      ((eql image1 image2)
       ;; We could most likely just return T here, but this might
       ;; be less error prone
       (call-next-method))
      (cache
       (image-equal-cache-result cache))
      (t
       (let ((res (call-next-method)))
         (bt:with-lock-held (*lock*)
           (or
            (image-equal-cache-for-key key)
            (make-instance 'image-equal-cache
                           :key key
                           :result res)))
         res)))))

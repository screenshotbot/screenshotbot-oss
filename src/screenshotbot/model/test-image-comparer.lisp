;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-image-comparer
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/magick/magick-lw
                #:get-non-alpha-pixels)
  (:import-from #:screenshotbot/model/image
                #:mask-rect
                #:image-hash
                #:base-image-comparer
                #:make-image
                #:image=)
  (:import-from #:screenshotbot/model/image-comparer
                #:image-equal-cache
                #:compare-threshold
                #:make-image-comparer
                #:threshold-comparer)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:screenshotbot/model/testing
                #:with-test-image))
(in-package :screenshotbot/model/test-image-comparer)

(util/fiveam:def-suite)

(def-fixture state (&key (threshold 0))
  (with-test-store ()
    (with-installation ()
      (let ((comparer (make-instance 'threshold-comparer :threshold threshold)))
        (&body)))))

(test test-with-test-image
  (with-fixture state ()
    (with-test-image (name)
      (pass))))

(test compare-two-identical-images
  (with-fixture state ()
    (with-test-image (f1 :pixels '((1 2)))
      (with-test-image (f2 :pixels '((1 2)))
        (is-true
         (image=
          comparer
          (make-image :pathname f1)
          (make-image :pathname f2)
          nil))))))

(test compare-two-unidentical-images
  (with-fixture state (:threshold 0)
    (with-test-image (f1 :pixels '((1 2)))
      (with-test-image (f2)
        (is-false
         (image=
          comparer
          (make-image :pathname f1)
          (make-image :pathname f2)
          nil))))))

(test with-comparison-threshold
  (with-fixture state (:threshold 1/100)
    (with-test-image (f1 :pixels '((1 2)))
      (with-test-image (f2)
        (is-true
         (image=
          comparer
          (make-image :pathname f1)
          (make-image :pathname f2)
          nil))))))

(test with-ridiculously-low-threshold
  (with-fixture state (:threshold 1/10000)
    (with-test-image (f1 :pixels '((1 2)))
      (with-test-image (f2)
        (is-false
         (image=
          comparer
          (make-image :pathname f1)
          (make-image :pathname f2)
          nil))))))

(test with-comparison-threshold-but-more-failures
  (with-fixture state (:threshold 1/100)
    (with-test-image (f1 :pixels '((1 2)))
      (with-test-image (f2 :pixels '((3 3)))
        (is-false
         (image=
          comparer
          (make-image :pathname f1)
          (make-image :pathname f2)
          nil))))))

(test different-dimensions-is-always-false
  (with-fixture state (:threshold 1/2)
    (with-test-image (f1 :height 9)
      (with-test-image (f2)
        (is-false
         (image=
          comparer
          (make-image :pathname f1)
          (make-image :pathname f2)
          nil))))))

(test make-image-comparer-when-no-threshold
  (with-fixture state ()
    (assert-that
     (make-image-comparer (make-recorder-run))
     (has-typep 'base-image-comparer))))

(test make-image-comparer-when-zero-threshold
  (with-fixture state ()
    (assert-that
     (make-image-comparer (make-recorder-run
                           :compare-threshold 0.0))
     (has-typep 'base-image-comparer))))

(test make-image-comparer-when-non-zero-threshold
  (with-fixture state ()
    (let ((comparer (make-image-comparer (make-recorder-run
                                         :compare-threshold 0.001))))
      (assert-that
       comparer
       (has-typep 'threshold-comparer))
      (assert-that
       (compare-threshold comparer)
       (is-equal-to 0.001)))))

(test threshold-image-comparison-is-cached
  (with-fixture state ()
    (with-test-image (file1)
      (with-test-image (file2 :pixels '((5 5)))
        (let ((im1 (make-image :pathname file1))
              (im2 (make-image :pathname file2))
              (comparer (make-image-comparer (make-recorder-run
                                              :compare-threshold 0.001)))
              (counter 0))
          (is (not (eql im1 im2)))
          (is (not (equalp (image-hash im1) (image-hash im2))))
          (is-false (image= (make-instance 'base-image-comparer)
                            im1 im2 nil))
          (cl-mock:with-mocks ()
            (cl-mock:if-called 'get-non-alpha-pixels
                               (lambda (&rest args)
                                 (incf counter)
                                 (make-array '(2 2))))
            (is-false (image= comparer im1 im2 nil))
            (is (eql 1 counter))
            (is-false (image= comparer im1 im2 nil))
            (assert-that
             counter
             (described-as "The image processing must be cached the second time"
               (is-equal-to 1)))
            (assert-that
             (class-instances 'image-equal-cache)
             (has-length 1))))))))

(test threshold-image-comparison-is-cached-when-same-too
  (with-fixture state ()
    (with-test-image (file1)
      (with-test-image (file2 :pixels '((5 5)))
        (let ((im1 (make-image :pathname file1))
              (im2 (make-image :pathname file2))
              (comparer (make-image-comparer (make-recorder-run
                                              :compare-threshold 0.1)))
              (counter 0))
          (is (not (eql im1 im2)))
          (is (not (equalp (image-hash im1) (image-hash im2))))
          (is-false (image= (make-instance 'base-image-comparer)
                            im1 im2 nil))
          (cl-mock:with-mocks ()
            (cl-mock:if-called 'get-non-alpha-pixels
                               (lambda (&rest args)
                                 (incf counter)
                                 (make-array '(2 2))))
            (is-true (image= comparer im1 im2 nil))
            (is (eql 1 counter))
            (is-true (image= comparer im1 im2 nil))
            (assert-that
             counter
             (described-as "The image processing must be cached the second time"
               (is-equal-to 1)))
            (assert-that
             (class-instances 'image-equal-cache)
             (has-length 1))))))))

(test images-that-are-same-are-not-cached
  (with-fixture state ()
    (with-test-image (file1)
      (let ((im1 (make-image :pathname file1))
            (comparer (make-image-comparer (make-recorder-run
                                            :compare-threshold 0.1))))
        (is-true (image= comparer im1 im1 nil))
        (assert-that
         (class-instances 'image-equal-cache)
         (has-length 0))))))

(test negative-mask-coordinates-bug
  "Demonstrates bug with negative mask coordinates being treated as unsigned.
   Two images differ at pixel (135,416) but the mask with negative coordinates
   should cover the entire image, making them equal."
  (with-fixture state ()
    (with-test-image (f1 :width 2118 :height 2532)
      (with-test-image (f2 :width 2118 :height 2532
                           :pixels '((135 416)))
        (let* ((im1 (make-image :pathname f1))
               (im2 (make-image :pathname f2))
               ;; Mask that should cover entire image (from user's bug report)
               (mask (make-instance 'mask-rect
                                    :left -388.29245
                                    :top -81.14493
                                    :width 2703.924
                                    :height 2707.5926))
               (masks (list mask)))
          ;; The mask covers from (-388.29, -81.14) to (2315.63, 2626.45)
          ;; which should fully cover the 2118x2532 image
          ;; Therefore the single pixel difference at (135, 416) is masked
          ;; and the images should be considered equal
          (is-true
           (image=
            comparer
            im1
            im2
            masks)
           "Images with masked difference should be equal, but negative mask coordinates are treated as unsigned size_t, causing them to wrap to huge positive values"))))))

(test negative-mask-coordinates-bug--ensure-0-0-is-still-under-mask
  "Demonstrates bug with negative mask coordinates being treated as unsigned.
   Two images differ at pixel (135,416) but the mask with negative coordinates
   should cover the entire image, making them equal."
  (with-fixture state ()
    (with-test-image (f1 :width 2118 :height 2532)
      (with-test-image (f2 :width 2118 :height 2532
                           :pixels '((0 0)))
        (let* ((im1 (make-image :pathname f1))
               (im2 (make-image :pathname f2))
               ;; Mask that should cover entire image (from user's bug report)
               (mask (make-instance 'mask-rect
                                    :left -388.29245
                                    :top -81.14493
                                    :width 2703.924
                                    :height 2707.5926))
               (masks (list mask)))
          ;; The mask covers from (-388.29, -81.14) to (2315.63, 2626.45)
          ;; which should fully cover the 2118x2532 image
          ;; Therefore the single pixel difference at (135, 416) is masked
          ;; and the images should be considered equal
          (is-true
           (image=
            comparer
            im1
            im2
            masks)
           "Images with masked difference should be equal, but negative mask coordinates are treated as unsigned size_t, causing them to wrap to huge positive values"))))))

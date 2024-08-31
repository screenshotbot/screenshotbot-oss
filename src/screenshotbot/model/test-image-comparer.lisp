;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-image-comparer
  (:use #:cl
        #:fiveam)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/magick/magick-lw
                #:get-non-alpha-pixels
                #:save-as-webp
                #:with-pixel
                #:screenshotbot-set-pixel
                #:magick-new-image
                #:with-wand
                #:pixel-set-color
                #:new-pixel-wand)
  (:import-from #:screenshotbot/model/image
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
  (:import-from #:util/testing
                #:with-global-binding)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:fiveam-matchers/has-length
                #:has-length))
(in-package :screenshotbot/model/test-image-comparer)

(util/fiveam:def-suite)

(def-easy-macro with-test-image (&binding name &key pixels (color "red")
                                          (height 10)
                                          (width 10)
                                          &fn fn)
  (uiop:with-temporary-file (:pathname p :type "webp")
    (with-wand (wand)
     (let ((default-pixel (new-pixel-wand)))
       (pixel-set-color default-pixel "none")
       (magick-new-image wand width height default-pixel)
       (loop for (x y) in pixels
             do
                (with-pixel (pixel x y)
                  (screenshotbot-set-pixel wand pixel color)))
       (save-as-webp wand p)
       (fn p)))))

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

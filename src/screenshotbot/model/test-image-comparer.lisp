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
                #:save-as-webp
                #:with-pixel
                #:screenshotbot-set-pixel
                #:magick-new-image
                #:with-wand
                #:pixel-set-color
                #:new-pixel-wand)
  (:import-from #:screenshotbot/model/image
                #:make-image
                #:image=)
  (:import-from #:screenshotbot/model/image-comparer
                #:threshold-comparer)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/async
                #:*magick-kernel*
                #:with-magick-kernel)
  (:import-from #:util/testing
                #:with-global-binding))
(in-package :screenshotbot/model/test-image-comparer)

(util/fiveam:def-suite)

(def-easy-macro with-test-image (&binding name &key pixels (color "red")
                                          (height 10)
                                          (width 10)
                                          &fn fn)
  (uiop:with-temporary-file (:pathname p :type "webp")
    (let ((default-pixel (new-pixel-wand)))
      (pixel-set-color default-pixel "none")
      (with-wand (wand)
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
        (with-global-binding ((*magick-kernel* nil))
          (unwind-protect
               (&body)
            (with-magick-kernel ()
              (lparallel:end-kernel :wait t))))))))

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

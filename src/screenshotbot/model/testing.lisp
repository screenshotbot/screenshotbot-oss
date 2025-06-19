;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/testing
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/magick/magick-lw
                #:save-as-webp
                #:magick-new-image
                #:pixel-set-color
                #:new-pixel-wand
                #:with-wand
                #:with-pixel
                #:screenshotbot-set-pixel))
(in-package :screenshotbot/model/testing)

(def-easy-macro with-test-image (&binding name &key pixels (color "red")
                                          (height 10)
                                          (width 10)
                                          &fn fn)
  "Creates a temporary WebP test image with specified pixels and dimensions.
   PIXELS is a list of (x y) coordinate pairs to set to COLOR.
   Binds NAME to the pathname of the temporary image file."
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




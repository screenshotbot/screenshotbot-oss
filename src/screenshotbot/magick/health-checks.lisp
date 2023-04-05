;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick/health-checks
  (:use #:cl)
  (:import-from #:util/health-check
                #:def-health-check)
  (:import-from #:screenshotbot/magick/magick-lw
                #:map-non-alpha-pixels
                #:magick-write-image
                #:with-wand)
  (:import-from #:easy-macros
                #:def-easy-macro))
(in-package :screenshotbot/magick/health-checks)

(defun read-file-sequence (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((res (make-array (file-length stream)
                           :element-type '(unsigned-byte 8))))
      (read-sequence res stream)
      res)))

(defvar *rose* (read-file-sequence (asdf:system-relative-pathname
                                    :screenshotbot "fixture/rose.webp")))

(def-easy-macro with-rose-wand (&binding wand &fn fn)
  (uiop:with-temporary-file (:stream webp-s :pathname webp :type "webp"
                              :element-type '(unsigned-byte 8))
    (write-sequence *rose* webp-s)
    (finish-output webp-s)
    (with-wand (wand :file webp)
      (fn wand))))

(def-health-check load-webp-and-save-png ()
  (handler-bind ((error (lambda (e)
                          #+lispworks
                          (dbg:output-backtrace :brief))))
    (with-rose-wand (wand)
      (uiop:with-temporary-file (:pathname png :type "png")
        (magick-write-image wand (namestring png))))))

(def-health-check map-non-alpha-pixels ()
  (with-rose-wand (wand)
    (map-non-alpha-pixels wand (lambda (x y)
                                 (declare (ignore x y))))))

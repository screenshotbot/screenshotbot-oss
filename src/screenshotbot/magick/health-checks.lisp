;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick/health-checks
  (:use #:cl)
  (:import-from #:util/health-check
                #:def-health-check)
  (:import-from #:screenshotbot/magick/magick
                #:*magick*)
  (:import-from #:screenshotbot/magick/magick-lw
                #:ping-image-metadata
                #:save-wand-to-file
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

(defvar *heic* (read-file-sequence (asdf:system-relative-pathname
                                    :screenshotbot "fixture/rose.heic")))

(def-easy-macro with-rose-wand (&binding wand &key (content *rose*) &fn fn)
  (uiop:with-temporary-file (:stream webp-s :pathname webp :type "webp"
                              :element-type '(unsigned-byte 8))
    (write-sequence content webp-s)
    (finish-output webp-s)
    (with-wand (wand :file webp)
      (fn wand))))

(def-health-check load-webp-and-save-png ()
  (handler-bind ((error (lambda (e)
                          #+lispworks
                          (dbg:output-backtrace :brief))))
    (with-rose-wand (wand)
      (uiop:with-temporary-file (:pathname png :type "png")
        (save-wand-to-file wand (namestring png))
        (assert-equal "PNG" (%image-format png))))))

(def-health-check load-heic-and-save-png ()
  (handler-bind ((error (lambda (e)
                          #+lispworks
                          (dbg:output-backtrace :brief))))
    (with-rose-wand (wand :content *heic*)
      (uiop:with-temporary-file (:pathname png :type "png")
        (save-wand-to-file wand (namestring png))
        (assert-equal "PNG" (%image-format png))))))

(defun %image-format (file)
  (third (ping-image-metadata *magick* file)))

(defun assert-equal (one two)
  (unless (equal one two)
    (error "failed: ~s is not equal to ~s"
           one two)))

(def-health-check load-webp-and-save-heic ()
  (handler-bind ((error (lambda (e)
                          #+lispworks
                          (dbg:output-backtrace :brief))))
    (with-rose-wand (wand)
      (uiop:with-temporary-file (:pathname png :type "heic")
        (save-wand-to-file wand (namestring png))
        (assert-equal "HEIC" (%image-format png))))))

(def-health-check load-webp-and-save-jxl ()
  (handler-bind ((error (lambda (e)
                          (format t "error: ~a~%" e)
                          #+lispworks
                          (dbg:output-backtrace :brief))))
    (with-rose-wand (wand)
      (uiop:with-temporary-file (:pathname png :type "jxl")
        (save-wand-to-file wand (namestring png))
        (assert-equal "JXL" (%image-format png))))))

(def-health-check map-non-alpha-pixels ()
  (with-rose-wand (wand)
    (map-non-alpha-pixels wand (lambda (x y)
                                 (declare (ignore x y))))))

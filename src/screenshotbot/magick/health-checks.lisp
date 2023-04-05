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
                #:magick-write-image
                #:with-wand))
(in-package :screenshotbot/magick/health-checks)

(defun read-file-sequence (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((res (make-array (file-length stream)
                           :element-type '(unsigned-byte 8))))
      (read-sequence res stream)
      res)))

(defvar *rose* (read-file-sequence (asdf:system-relative-pathname
                                    :screenshotbot "fixture/rose.webp")))

(def-health-check load-webp-and-save-png ()
  (handler-bind ((error (lambda (e)
                          #+lispworks
                          (dbg:output-backtrace :brief))))
   (uiop:with-temporary-file (:stream webp-s :pathname webp :type "webp"
                              :element-type '(unsigned-byte 8))
     (write-sequence *rose* webp-s)
     (finish-output webp-s)
     (uiop:with-temporary-file (:pathname png :type "png")
       (with-wand (wand :file webp)
         (magick-write-image wand (namestring png)))))))

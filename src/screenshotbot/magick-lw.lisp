;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick-lw
  (:use #:cl
        #:screenshotbot/magick)
  (:import-from #:screenshotbot/magick
                #:*magick*
                #:abstract-magick)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:compare-images
   #:compare-image-files))
(in-package :screenshotbot/magick-lw)

(defclass magick-native (abstract-magick)
  ())

(fli:register-module :magicd-wand :real-name "libMagickWand-7.Q8.so")

(fli:define-c-struct wand
    (dummy :int))

(fli:define-foreign-function (magick-wand-genesis "MagickWandGenesis")
    ()
  :result-type :int)

(fli:define-foreign-function (new-magick-wand "NewMagickWand")
    ()
  :result-type (:pointer wand))

(fli:define-foreign-function (destroy-magick-wand "DestroyMagickWand")
    ((wandp :pointer))
  :result-type (:pointer wand))

(fli:define-foreign-function (magick-read-image "MagickReadImage")
    ((wand :pointer)
     (filename (:reference-pass :ef-mb-string)))
  :result-type :boolean)

(fli:define-foreign-function (magick-compare-images "MagickCompareImages" )
  ((wand (:pointer wand))
   (reference-wand (:pointer wand))
   (metric :int)
   (output (:reference-return :double)))
  :result-type (:pointer wand))

(fli:define-foreign-function (magick-set-option "MagickSetOption")
    ((wand (:pointer wand))
     (name (:reference :ef-mb-string))
     (value (:reference :ef-mb-string)))
  :result-type :boolean)

(fli:define-foreign-function (magick-write-image "MagickWriteImage")
    ((wand (:pointer wand))
     (file (:reference :ef-mb-string)))
  :result-type :boolean)

;; Look at compare.h in MagickCore
(defvar +root-mean-squared-error-metric+ 10)

(defvar *magick-wand-inited* nil)

(defun mb (x)
  (format nil "~a" (* x 1024 1024)))

(defun init-magick-wand ()
  (unless *magick-wand-inited*
    (setf (uiop:getenv "MAGICK_MEMORY_LIMIT") (mb 30))
    (setf (uiop:getenv "MAGICK_MAP_LIMIT") (mb 200))
    (setf (uiop:getenv "MAGICK_DISK_LIMIT") (mb 1000))
    (setf *magick-wand-inited* t)
    (magick-wand-genesis)))

(defmacro with-wand ((wand file) &body body)
  `(call-with-wand
    ,file (lambda (,wand) ,@body)))

(defun call-with-wand (file fn)
  (init-magick-wand)

  (cond
    ((or (stringp file)
         (pathnamep file))
     (call-with-wand (make-file-wand file) fn))
    (t
     (let ((wand file))
      (unwind-protect
           (funcall fn wand)
        (destroy-magick-wand wand))))))

(defun make-file-wand (file)
  (let ((wand (new-magick-wand)))
    (magick-read-image wand (namestring file))
    wand))

(defun compare-images (wand1 wand2)
  (multiple-value-bind (output difference)
      (magick-compare-images
       wand1
       wand2
       +root-mean-squared-error-metric+
       0.0)
    (unwind-protect
         (eql difference 0.0)
      (unless (fli:null-pointer-p output)
       (destroy-magick-wand output)))))

(defmethod compare-image-files ((magick magick-native) file1 file2)
  (with-wand (wand1 file1)
    (with-wand (wand2  file2)
      (compare-images wand1 wand2))))


(defmacro check-boolean (x)
  `(assert ,x))

(defmethod convert-to-lossless-webp ((self magick-native) input output)
  (with-wand (wand input)
    (check-boolean (magick-set-option wand "webp:lossless" "true"))
    (check-boolean (magick-write-image wand (namestring output)))))

(setf *magick* (make-instance 'magick-native))

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
   #:compare-image-files
   #:magick-exception
   #:magick-exception-message))
(in-package :screenshotbot/magick-lw)

(defclass magick-native (abstract-magick)
  ())

(fli:define-c-typedef magick-size-type :uint64)

(fli:register-module :magicd-wand :real-name "libMagickWand-7.Q8.so")

(fli:define-c-struct wand
    (dummy :int))

(fli:define-foreign-function (magick-wand-genesis "MagickWandGenesis")
    ()
  :result-type :int)

(fli:define-foreign-function (magick-wand-terminus "MagickWandTerminus")
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

(fli:define-foreign-function (magick-ping-image "MagickPingImage")
    ((wand (:pointer wand))
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

(fli:define-foreign-function (magick-clear-exception "MagickClearException")
    ((wand (:pointer wand)))
  :result-type :boolean)

(fli:define-c-enum resource-type
  UndefinedResource
  AreaResource
  DiskResource
  FileResource
  HeightResource
  MapResource
  MemoryResource
  ThreadResource
  ThrottleResource
  TimeResource
  WidthResource
  ListLengthResource)

(fli:define-c-enum exception-type
    UndefinedException
  (  ResourceLimitWarning 300)
  (  WarningException 300)
  (  TypeWarning 305)
  (  OptionWarning 310)
  (  DelegateWarning 315)
  (  MissingDelegateWarning 320)
  (  CorruptImageWarning 325)
  (  FileOpenWarning 330)
  (  BlobWarning 335)
  (  StreamWarning 340)
  (  CacheWarning 345)
  (  CoderWarning 350)
  (  FilterWarning 352)
  (  ModuleWarning 355)
  (  DrawWarning 360)
  (  ImageWarning 365)
  (  WandWarning 370)
  (  RandomWarning 375)
  (  XServerWarning 380)
  (  MonitorWarning 385)
  (  RegistryWarning 390)
  (  ConfigureWarning 395)
  (  PolicyWarning 399)
  (  ErrorException 400)
  (  ResourceLimitError 400)
  (  TypeError 405)
  (  OptionError 410)
  (  DelegateError 415)
  (  MissingDelegateError 420)
  (  CorruptImageError 425)
  (  FileOpenError 430)
  (  BlobError 435)
  (  StreamError 440)
  (  CacheError 445)
  (  CoderError 450)
  (  FilterError 452)
  (  ModuleError 455)
  (  DrawError 460)
  (  ImageError 465)
  (  WandError 470)
  (  RandomError 475)
  (  XServerError 480)
  (  MonitorError 485)
  (  RegistryError 490)
  (  ConfigureError 495)
  (  PolicyError 499)
  (  FatalErrorException 700)
  (  ResourceLimitFatalError 700)
  (  TypeFatalError 705)
  (  OptionFatalError 710)
  (  DelegateFatalError 715)
  (  MissingDelegateFatalError 720)
  (  CorruptImageFatalError 725)
  (  FileOpenFatalError 730)
  (  BlobFatalError 735)
  (  StreamFatalError 740)
  (  CacheFatalError 745)
  (  CoderFatalError 750)
  (  FilterFatalError 752)
  (  ModuleFatalError 755)
  (  DrawFatalError 760)
  (  ImageFatalError 765)
  (  WandFatalError 770)
  (  RandomFatalError 775)
  (  XServerFatalError 780)
  (  MonitorFatalError 785)
  (  RegistryFatalError 790)
  (  ConfigureFatalError 795)
  (  PolicyFatalError 799 ))

(fli:define-foreign-function (magick-get-exception "MagickGetException")
    ((wand (:pointer wand))
     (exception-type (:reference-return exception-type)))
  :result-type (:pointer :char))


(fli:define-foreign-function (magick-get-resource-limit "MagickGetResourceLimit")
    ((op resource-type))
  :result-type magick-size-type)

(fli:define-foreign-function (magick-get-resource "MagickGetResource")
    ((op resource-type))
  :result-type magick-size-type)

(fli:define-foreign-function (magick-set-resource-limit "MagickSetResourceLimit")
    ((op resource-type)
     (limit magick-size-type))
  :result-type :boolean)

(fli:define-foreign-function (magick-get-image-height "MagickGetImageHeight")
    ((wand (:pointer wand)))
  :result-type :size-t)

(fli:define-foreign-function (magick-get-image-width "MagickGetImageWidth")
    ((wand (:pointer wand)))
  :result-type :size-t)

(defvar +area-resource+ 1)

(fli:define-foreign-function (magick-strip-image "MagickStripImage")
    ((wand (:pointer wand)))
  :result-type :boolean)

;; Look at compare.h in MagickCore
(defvar +root-mean-squared-error-metric+ 10)

(defvar *magick-wand-inited* nil)

(defun mb (x)
  (format nil "~a" (* x 1024 1024)))

(defun init-magick-wand ()
  (unless *magick-wand-inited*
    (magick-wand-genesis)
    (update-resource-limits)
    (setf *magick-wand-inited* t)))

(define-condition magick-exception (error)
  ((expression :initarg :expression)
   (message :initarg :message
            :reader magick-exception-message)))

(defmethod print-object ((self magick-exception) out)
  (with-slots (message) self
   (format out "Magick exception: ~a" message)))

(defmacro check-boolean (x wand)
  (let ((wand-sym (gensym "wand")))
    `(let ((,wand-sym ,wand))
       (unless ,x
         (cond
           (,wand-sym
            (raise-magick-exception ,wand-sym ',x))
           (t
            (error "expression failed")))))))

(defun raise-magick-exception (wand &optional expression)
  (multiple-value-bind (message type)
      (magick-get-exception wand 'UndefinedException)
    (declare (ignore type))
    (magick-clear-exception wand)
    (error 'magick-exception
            :expression expression
            :message (fli:convert-from-foreign-string message))))

(defun update-resource-limits ()
  (loop for (name value) in `((AreaResource 3000000)
                              (DiskResource ,(* 1000 1024 1024))
                              (WidthResource 10000)
                              (HeightResource 40000)
                              (ListLengthResource ,(* 10000 1024  1024))
                              (MemoryResource ,(* 200 1024 1024)))
        do
           (check-boolean (magick-set-resource-limit name value)
                          nil)))

(defun end-magick-wand ()
  (progn
    (magick-wand-terminus)
    (setf *magick-wand-inited* nil)))

(defmacro with-wand ((wand &key file) &body body)
  `(call-with-wand
    ,file (lambda (,wand) ,@body)))

(defun call-with-wand (file fn)
  (init-magick-wand)
  (let ((wand (new-magick-wand)))
    (unwind-protect
         (progn
           (when file
             (check-boolean (magick-read-image wand (namestring file)) wand))
           (funcall fn wand))
      (destroy-magick-wand wand))))

(defun compare-images (wand1 wand2)
  (assert (not (fli:null-pointer-p wand1)))
  (assert (not (fli:null-pointer-p wand2)))
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
  (with-wand (wand1 :file file1)
    (with-wand (wand2 :file file2)
      (compare-images wand1 wand2))))



(defmethod convert-to-lossless-webp ((self magick-native) input output)
  (with-wand (wand :file input)
    (check-boolean (magick-set-option wand "webp:lossless" "true") wand)
    (check-boolean (magick-strip-image wand) wand)
    (check-boolean (magick-write-image wand (namestring output)) wand)))

(setf *magick* (make-instance 'magick-native))


(defmethod ping-image-dimensions ((magick magick-native) file)
  (with-wand (wand)
    (check-boolean (magick-ping-image wand (namestring file))
                   wand)
    (list
     (magick-get-image-width wand)
     (magick-get-image-height wand))))

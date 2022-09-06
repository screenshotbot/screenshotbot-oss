;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick/magick-lw
  (:use #:cl
        #:screenshotbot/magick/magick)
  (:import-from #:screenshotbot/magick
                #:with-magick-gatekeeper
                #:*magick*
                #:abstract-magick)
  (:import-from #:util/macros
                #:def-easy-macro)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#-lispworks #:fli #:util/fake-fli))
  (:export
   #:compare-images
   #:compare-image-files
   #:magick-exception
   #:magick-exception-message
   #:map-non-alpha-pixels
   #:get-non-alpha-pixels))
(in-package :screenshotbot/magick/magick-lw)

(defclass magick-native (abstract-magick)
  ())

(fli:define-c-typedef magick-size-type :uint64)
(fli:define-c-typedef quantum :uint8)

(defvar *windows-magick-dir*
  #P"c:/Program Files/ImageMagick-7.1.0-Q8/")

(defun win-magick-lib (name)
  (namestring (pathname (path:catfile *windows-magick-dir* (format nil "CORE_RL_~a_.dll" name)))))

(defvar *path-setp* nil)

(defun register-magick-wand ()
  (when (uiop:os-windows-p)
    (unless *path-setp*
      (setf (uiop:getenv "Path") (format nil "~a;~a" (namestring *windows-magick-dir*) (uiop:getenv "Path")))
      (setf *path-setp* t)))

  (fli:register-module
   ;; TODO: typo in module name, we should fix when a restart is due.
   :magicd-wand
   :file-name
   (cond
    ((uiop:os-windows-p)
     ;; TODO: if needed we can discover this from the registry, see the
     ;; python wand code that does the same
     (win-magick-lib "MagickWand"))
    (t "libMagickWand-7.Q8.so"))))

;; (fli:disconnect-module :magicd-wand)

(fli:register-module :magick-native
                     :real-name
                     (asdf:output-file
                         'asdf:compile-op
                          (asdf:find-component :screenshotbot '("magick" "magick-native"))))


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

(fli:define-c-enum alpha-channel-option
    UndefinedAlphaChannel
  ActivateAlphaChannel
  AssociateAlphaChannel
  BackgroundAlphaChannel
  CopyAlphaChannel
  DeactivateAlphaChannel
  DiscreteAlphaChannel
  DisassociateAlphaChannel
  ExtractAlphaChannel
  OffAlphaChannel
  OnAlphaChannel
  OpaqueAlphaChannel
  RemoveAlphaChannel
  SetAlphaChannel
  ShapeAlphaChannel
  TransparentAlphaChannel)

(fli:define-foreign-function (%get-policy-value "GetPolicyValue")
    ((name (:reference-pass :ef-mb-string)))
  :result-type (:pointer :char)
  :documentation "For debugging only")

(fli:define-foreign-function (get-max-memory-request "GetMaxMemoryRequest")
    ()
  :result-type :size-t
  :documentation "For debugging only")

(defun get-policy-value (name)
  (let ((ret (%get-policy-value name)))
    (unless (fli:null-pointer-p ret)
     (unwind-protect
          (fli:convert-from-foreign-string ret)
       (magick-relinquish-memory ret)))))

(fli:define-foreign-function (magick-get-exception "MagickGetException")
    ((wand (:pointer wand))
     (exception-type (:reference-return exception-type)))
  :result-type (:pointer :char))

(fli:define-foreign-function (magick-relinquish-memory "MagickRelinquishMemory")
  ((resource (:pointer :void)))
  :result-type :void)

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

(fli:define-foreign-function (magick-get-image-alpha-channel "MagickGetImageAlphaChannel")
    ((wand (:pointer wand)))
    :result-type :boolean)

(fli:define-foreign-function (magick-set-image-alpha-channel "MagickSetImageAlphaChannel")
                             ((wand (:pointer wand))
                              (alpha-channel-option alpha-channel-option))
    :result-type :boolean)

;; Look at compare.h in MagickCore
(defvar +root-mean-squared-error-metric+ 10)

(defvar *magick-wand-inited* nil)

(defun mb (x)
  (format nil "~a" (* x 1024 1024)))

(defun init-magick-wand ()
  (unless *magick-wand-inited*
    (register-magick-wand)
    #+lispworks
    (screenshotbot/magick/memory:update-magick-memory-methods)
    (magick-wand-genesis)
    (update-resource-limits)
    (setf *magick-wand-inited* t)))

(defun end-magick-wand ()
  (progn
    (magick-wand-terminus)
    (setf *magick-wand-inited* nil)))

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
  (declare (optimize (debug 3) (speed 0)))
  (multiple-value-bind (message type)
      (magick-get-exception wand 'UndefinedException)
    (declare (ignore type))
    (magick-clear-exception wand)
    (unwind-protect
         (error 'magick-exception
                 :expression expression
                 :message (fli:convert-from-foreign-string message))
      (magick-relinquish-memory message))))

(defun update-resource-limits ()
  (loop for (name value) in `((AreaResource 3000000)
                              (DiskResource ,(* 1000 1024 1024))
                              (WidthResource 10000)
                              (HeightResource 40000)
                              (ListLengthResource ,(* 10000 1024  1024))
                              (MemoryResource ,(* 2000 1024 1024)))
        do
           (check-boolean (magick-set-resource-limit name value)
                          nil)))

(def-easy-macro with-wand (&binding wand &key file from (alpha t) &fn fn)
  (init-magick-wand)
  (let ((wand (or from (new-magick-wand))))
    (unwind-protect
         (progn
             (when file
               (check-boolean (magick-read-image wand (namestring file)) wand)
               (when alpha
                 (check-boolean (magick-set-image-alpha-channel wand 'OnAlphaChannel)
                                wand)))
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
       0.0d0)
    (unwind-protect
         (= difference 0.0d0)
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



(fli:define-c-struct pixel
    (x :size-t)
  (y :size-t))


(fli:define-foreign-function screenshotbot-find-non-transparent-pixels
     ((wand (:pointer wand))
      (output (:pointer pixel))
      (max :size-t))
     :result-type :size-t)

;; (fli:disconnect-module :magick-native)


(defun map-non-alpha-pixels (wand fn &key (limit 1000))
  ;; for each pixel in wand that is not 100% transparent, call the
  ;; function, upto LIMIT times.
  (let ((pxs (get-non-alpha-pixels wand :limit limit)))
    (loop for i below (car (array-dimensions pxs))
          do (funcall fn (aref pxs i 0) (aref pxs i 1)))))

(defun get-non-alpha-pixels (wand &key (limit 1000))
  (declare (optimize (Debug 3) (speed 0)))
  (with-magick-gatekeeper ()
   (when (magick-get-image-alpha-channel wand)
     (fli:with-dynamic-foreign-objects
         ((output pixel :nelems limit))
       (let ((size (screenshotbot-find-non-transparent-pixels
                    wand output limit)))
         (let ((ret (make-array (list size 2))))
           (loop for i below size
                 do
                    (setf (aref ret i 0) (fli:foreign-slot-value output #-lispworks 'pixel 'x))
                    (setf (aref ret i 1) (fli:foreign-slot-value output #-lispworks 'pixel 'y))
                    (fli:incf-pointer output))
           ret))))))

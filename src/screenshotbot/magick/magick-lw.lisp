;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick/magick-lw
  (:use #:cl
        #:screenshotbot/magick/magick
        #+magick-7
        #:screenshotbot/magick/ffi-7
        #+magick-6
        #:screenshotbot/magick/ffi-6
        #:screenshotbot/mask-rect-api)
  (:import-from #:screenshotbot/magick
                #:with-magick-gatekeeper
                #:*magick*
                #:abstract-magick)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:screenshotbot/magick/build
                #:magick-lib-suffix)
  (:import-from #:util/copy-file
                #:copy-file-fast)
  (:import-from #:util/health-check
                #:def-health-check)
  (:import-from #:util/native-module
                #:make-system-module
                #:embed-module
                #:load-module
                #:make-native-module)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#-lispworks #:fli #:util/fake-fli))
  (:export
   #:compare-images
   #:compare-image-files
   #:magick-exception
   #:magick-exception-message
   #:map-non-alpha-pixels
   #:get-non-alpha-pixels
   #:ping-image-metadata
   #:with-image-comparison
   #:save-as-webp
   #:with-pixel-wand
   #:screenshotbot-set-pixel
   #:with-pixel
   #:embed-magick-native))
(in-package :screenshotbot/magick/magick-lw)

(defclass magick-native (abstract-magick)
  ())

(fli:define-c-typedef magick-size-type :uint64)
(fli:define-c-typedef quantum :uint8)

(defvar *windows-magick-dir*
  #P"c:/Program Files/ImageMagick-7.1.0-Q8/")

(defun win-magick-lib (name)
  (let ((ret (namestring (pathname (path:catfile *windows-magick-dir* (format nil "CORE_RL_~a_.dll" name))))))
    (assert (path:-e ret))
    ret))


(defvar *path-setp* nil)

(defun magick-so (name)
  (let ((suffix (magick-lib-suffix)))
   (lambda ()
     (cond
       ((uiop:os-windows-p)
        ;; TODO: if needed we can discover this from the registry, see the
        ;; python wand code that does the same
        (win-magick-lib name))
       (t (format nil "lib~a~a.so" name suffix))))))

(defvar *magick-wand*
  (make-system-module :magick-wand
                      :pathname-flag :file-name
                      :pathname-provider
                      (magick-so "MagickWand")))

(defvar *magick-core*
  (make-system-module :magick-core
                      :pathname-flag :file-name
                      :pathname-provider
                      (magick-so "MagickCore")))

(defvar *libwebp* (make-system-module :libwebp
                                      :file-name "libwebp.so"))
(defvar *libwebpmux* (make-system-module :libwebpmux
                                         :file-name "libwebpmux.so"))
(defvar *libwebpdemux* (make-system-module :libwebpdemux
                                         :file-name "libwebpdemux.so"))

(defvar *libpng* (make-system-module :libpng
                                     :file-name "libpng.so"))

(defvar *libgomp* (make-system-module :libgomp
                                      :file-name "libgomp.so.1"))

(defparameter *libs*
  ;; the order here matter!
  (list
   *libwebp*
   *libpng*
   *libwebpmux*
   *libwebpdemux*
   #+linux
   *libgomp*))

#+lispworks
(unless (hcl:delivered-image-p)
 (lw:define-action "Delivery actions" "Embed magick libraries"
   (lambda ()
     (mapc #'embed-module *libs*)
     (embed-module *magick-wand*)
     (embed-module *magick-core*)
     (embed-module *magick-native*))))

(defun register-magick-wand ()
  (when (uiop:os-windows-p)
    (unless *path-setp*
      (setf (uiop:getenv "Path") (format nil "~a;~a" (namestring *windows-magick-dir*) (uiop:getenv "Path")))
      (setf *path-setp* t)))

  #+lispworks
  (mapc #'load-module *libs*)

  (load-module *magick-core*)
  (load-module *magick-wand*)
  (load-magick-native))

(defun verify-magick ()
  (let ((ret (screenshotbot-verify-magick
              'SrcCompositeOp
              'SetAlphaChannel)))
    (unless (= ret 1)
      (error "The ImageMagick runtime does not match the configuration options that
 Screenshotbot was compiled against. This might happen if you
 recompiled or reinstalled ImageMagick, or switched the compiled
 assets to a different machine. (Got result: ~a)" ret))))


(defvar *magick-native* (make-native-module
                         :magick-native
                         :screenshotbot.magick
                         "magick-native"
                         :verify #'verify-magick))

(defun load-magick-native (&key force)
  #-linux
  (when force
    ;; For some reason reloading magick-native will cause segfaults on
    ;; mac tests.
    (warn "Can't reload magick-native on Mac"))
  (load-module *magick-native* :force force))

;; (load-magick-native :force t)

(fli:define-c-struct wand
    (dummy :int))

(fli:define-c-struct pixel-wand
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

(fli:define-foreign-function (magick-set-size "MagickSetSize")
    ((wand :pointer)
     (cols :size-t)
     (rows :size-t))
  :result-type :boolean
  :documentation "Only used for tests")

(fli:define-foreign-function (magick-crop-image "MagickCropImage")
    ((Wand :pointer)
     (width :size-t)
     (height :size-t)
     (x :size-t)
     (y :size-t))
  :result-type :boolean)

(fli:define-foreign-function (magick-new-image "MagickNewImage")
    ((wand (:pointer wand))
     (columns :size-t)
     (rows :size-t)
     (pixel-wand (:pointer pixel-wand)))
  :result-type :boolean)

(fli:define-foreign-function (new-pixel-wand "NewPixelWand")
    ()
  :result-type (:pointer pixel-wand))

(fli:define-foreign-function (pixel-set-alpha "PixelSetAlpha")
    ((pwand (:pointer pixel-wand))
     (alpha :double))
  :result-type :void)

(fli:define-foreign-function (pixel-set-color "PixelSetColor")
    ((pwand (:pointer pixel-wand))
     (color (:reference-pass :ef-mb-string)))
  :result-type :boolean)

(fli:define-foreign-function (clear-pixel-wand "ClearPixelWand")
    ((pixel-wand (:pointer pixel-wand)))
  :result-type :void)

(fli:define-foreign-function (magick-ping-image "MagickPingImage")
    ((wand (:pointer wand))
     (filename (:reference-pass :ef-mb-string)))
  :result-type :boolean)

(fli:define-foreign-function (magick-compare-images "MagickCompareImages" )
  ((wand (:pointer wand))
   (reference-wand (:pointer wand))
   (metric metric-type)
   (onutput (:reference-return :double)))
  :result-type (:pointer wand))

(fli:define-foreign-function (%magick-identify-image "MagickIdentifyImage")
    ((wand (:pointer wand)))
  :result-type (:pointer :char))

(fli:define-foreign-function (magick-set-option "MagickSetOption")
    ((wand (:pointer wand))
     (name (:reference :ef-mb-string))
     (value (:reference :ef-mb-string)))
  :result-type :boolean)

(fli:define-foreign-function (magick-set-image-artifact "MagickSetImageArtifact")
    ((wand (:pointer wand))
     (artifact (:reference :ef-mb-string))
     (value (:reference :ef-mb-string)))
  :result-type :boolean)

(fli:define-foreign-function (magick-write-image "MagickWriteImage")
    ((wand (:pointer wand))
     (file (:reference :ef-mb-string)))
  :result-type :boolean)

(fli:define-foreign-function (magick-clear-exception "MagickClearException")
    ((wand (:pointer wand)))
  :result-type :boolean)

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

(fli:define-foreign-function (magick-set-image-compose "MagickSetImageCompose")
    ((wand (:pointer wand))
     (compose composite-operator))
  :result-type :boolean)

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

(fli:define-foreign-function (magick-get-image-format "MagickGetImageFormat")
    ((wand (:pointer wand)))
  :result-type (:pointer :char))

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

(fli:define-foreign-function (magick-get-version "MagickGetVersion")
    ((version (:reference-return #-lispworks :uint64 #+lispworks :size-t)))
  :result-type (:pointer :char))

(defvar *magick-wand-inited* nil)

(defun mb (x)
  (format nil "~a" (* x 1024 1024)))

(defun init-magick-wand ()
  (unless *magick-wand-inited*
    (register-magick-wand)
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
                              (WidthResource 20000)
                              (HeightResource 40000)
                              (ListLengthResource ,(* 10000 1024  1024))
                              (MemoryResource ,(* 2000 1024 1024)))
        do
           (check-boolean (magick-set-resource-limit name value)
                          nil)))

(def-easy-macro with-wand (&binding wand &key file from (alpha t) &fn fn)
  (init-magick-wand)
  (push-event :magick.with-wand)
  (let ((wand (or from (new-magick-wand))))
    (unwind-protect
         (progn
             (when file
               (check-boolean (magick-read-image wand (namestring file)) wand)
               (when alpha
                 (set-wand-alpha-channel wand)))
           (funcall fn wand))
      (destroy-magick-wand wand))))

(fli:define-foreign-function screenshotbot-inplace-compare
    ((dest (:pointer wand))
     (src (:pointer wand)))
  :result-type :int)

(defun set-wand-alpha-channel (wand)
  (check-boolean (magick-set-image-alpha-channel wand 'SetAlphaChannel)
                 wand))


(def-easy-macro with-image-comparison (wand1 wand2 &key &binding result
                                             &binding same-p
                                             in-place-p
                                             (highlight-color "red")
                                             (lowlight-color "none")
                                             &fn fn)
  (assert (not (fli:null-pointer-p wand1)))
  (assert (not (fli:null-pointer-p wand2)))

  (cond
    (in-place-p
     (log:info "Using in-place image comparison")

     (flet ((inner (wand1 wand2)
              (set-wand-alpha-channel wand1)
              (let ((res (screenshotbot-inplace-compare wand1 wand2)))
                (when (< res 0)
                  (error "Error calling inplace-compare: ~a" res))
                (funcall fn wand1 (= res 0)))))
       (cond
         ((and
           (<
            (magick-get-image-height wand1)
            (magick-get-image-height wand2))
           (<
            (magick-get-image-width wand1)
            (magick-get-image-width wand2)))
          ;; if both the dimensions of wand1 are smaller than wand2, then
          ;; we swap them. Otherwise we might have a situation where a
          ;; larger image in both dimensions won't detect a change.
          (inner wand2 wand1))
         (t
          (inner wand1 wand2)))))
    (t
     (check-boolean (magick-set-image-artifact wand1 "compare:lowlight-color" lowlight-color) wand1)
     (check-boolean (magick-set-image-artifact wand1 "compare:highlight-color" highlight-color) wand1)
     (check-boolean (magick-set-image-compose wand1  'SrcCompositeOp) wand1)
     (multiple-value-bind (output difference)
         (magick-compare-images
          wand1
          wand2
          'RootMeanSquaredErrorMetric
          0.0d0)
       (unwind-protect
            (let ((same-p (= difference 0.0d0)))
              (funcall fn output same-p))
         (unless (fli:null-pointer-p output)
           (destroy-magick-wand output)))))))

(defun compare-images (wand1 wand2)
  (with-image-comparison (wand1 wand2 :same-p same-p)
    same-p))

(defmethod compare-image-files ((magick magick-native) file1 file2)
  (push-event :magick.compare-images-files)
  (with-wand (wand1 :file file1)
    (with-wand (wand2 :file file2)
      (compare-images wand1 wand2))))

(defun save-wand-to-file (wand output)
  "Similar to save as webp"
  (check-boolean (magick-write-image wand (namestring output)) wand))

(defun save-as-webp (wand output &key (lossless t))
  (when lossless
   (check-boolean (magick-set-option wand "webp:lossless" "true") wand))
  (check-boolean (magick-strip-image wand) wand)
  (save-wand-to-file wand output))

(defmethod convert-to-lossless-webp ((self magick-native) input output)
  (push-event :magick.convert-to-lossless-webp)
  (with-wand (wand :file input)
    (save-as-webp wand output)))


(setf *magick* (make-instance 'magick-native))


(defmethod ping-image-dimensions ((magick magick-native) file)
  (destructuring-bind (width height format)
      (ping-image-metadata magick file)
    (declare (ignore format))
    (list width height)))

(defmethod ping-image-metadata ((magick magick-native) file)
  (push-event :magick.ping-image-metadata)
  (with-wand (wand)
    (check-boolean (magick-ping-image wand (namestring file))
                   wand)
    (list
     (magick-get-image-width wand)
     (magick-get-image-height wand)
     (get-image-format wand))))

(defun get-image-format (wand)
  (let ((format (magick-get-image-format wand)))
    (unwind-protect
         (fli:convert-from-foreign-string format)
      (magick-relinquish-memory format))))

(fli:define-c-struct pixel
    (x :size-t)
  (y :size-t))

(def-easy-macro with-pixel (&binding pixel x y &fn fn)
  (fli:with-dynamic-foreign-objects
      ((pixel pixel))
    (setf (fli:foreign-slot-value pixel 'x) x)
    (setf (fli:foreign-slot-value pixel 'y) y)
    (funcall fn pixel)))

(fli:define-c-struct native-mask
    (x :size-t)
  (y :size-t)
  (width :size-t)
  (height :size-t))

(fli:define-foreign-function screenshotbot-set-pixel
    ((wand (:pointer wand))
     (pixel (:pointer pixel))
     (color (:reference-pass :ef-mb-string)))
  :result-type :boolean)

(fli:define-foreign-function screenshotbot-find-non-transparent-pixels-with-masks
    ((wand (:pointer wand))
     (masks (:pointer native-mask))
     (num-masks :size-t)
     (output (:pointer pixel))
     (max :size-t))
  :result-type :size-t)

(fli:define-foreign-function screenshotbot-verify-magick
    ((src-composite-op composite-operator)
     (on-alpha-channel alpha-channel-option))
  :result-type :int)


(defun map-non-alpha-pixels (wand fn &key (limit 1000))
  ;; for each pixel in wand that is not 100% transparent, call the
  ;; function, upto LIMIT times.
  (push-event :magick.map-non-alpha)
  (let ((pxs (get-non-alpha-pixels wand :limit limit)))
    (loop for i below (car (array-dimensions pxs))
          do (funcall fn (aref pxs i 0) (aref pxs i 1)))))

(def-easy-macro with-native-masks (&binding native-masks masks &fn fn)
  (fli:with-dynamic-foreign-objects ((native-masks native-mask :nelems (1+ (length masks))))
    (let ((ptr (fli:copy-pointer native-masks)))
      (loop for mask in masks
            do
               (progn
                 (setf (fli:foreign-slot-value ptr 'x) (ceiling (mask-rect-left mask)))
                 (setf (fli:foreign-slot-value ptr 'y)  (ceiling (mask-rect-top mask)))
                 (setf (fli:foreign-slot-value ptr 'width) (floor (mask-rect-width mask)))
                 (setf (fli:foreign-slot-value ptr 'height) (floor (mask-rect-height mask)))
                 (fli:incf-pointer ptr))))
    (fn native-masks)))

(defun get-non-alpha-pixels (wand &key (limit 1000)
                                    masks)
  (declare (optimize (Debug 3) (speed 0)))
  (log:debug "Got masks: ~S" (loop for mask in masks
                                   collect (list
                                            (mask-rect-left mask)
                                            (mask-rect-top mask)
                                            (mask-rect-width mask)
                                            (mask-rect-height mask))))
  (with-magick-gatekeeper ()
    (when (magick-get-image-alpha-channel wand)
      (with-native-masks (native-masks masks)
       (fli:with-dynamic-foreign-objects
           ((output pixel :nelems limit))
         (let ((size (screenshotbot-find-non-transparent-pixels-with-masks
                      wand
                      native-masks
                      (length masks)
                      output limit)))
           (let ((ret (make-array (list size 2))))
             (loop for i below size
                   do
                      (setf (aref ret i 0) (fli:foreign-slot-value output 'x))
                      (setf (aref ret i 1) (fli:foreign-slot-value output 'y))
                      (fli:incf-pointer output))
             ret)))))))

(defun limit-size-for-webp (wand)
  (let ((+max-dim+ 16383))
    (let ((width (magick-get-image-width wand))
          (height (magick-get-image-height wand)))
      (when (or
             (> width +max-dim+)
             (> height +max-dim+))
        (let ((width (min width +max-dim+))
              (height (min height +max-dim+)))
          (check-boolean
           (magick-crop-image wand
                              width
                              height
                              0
                              0)
           wand))))))

(defun compare-wands (before after p &key in-place-p)
  ;; Limit the size of the before and after wands before doing the
  ;; comparison
  (limit-size-for-webp before)
  (limit-size-for-webp after)
  (with-image-comparison (before after
                          :result result
                          :same-p same-p
                          :in-place-p in-place-p
                          :highlight-color "red"
                          :lowlight-color "none")
    (save-as-webp result p)
    same-p))

(def-easy-macro with-pixel-wand (&binding pixel-wand &fn fn)
  (let ((pwand (new-pixel-wand)))
    (unwind-protect
         (funcall fn pwand)
      (clear-pixel-wand pwand))))

(fli:define-foreign-function screenshotbot-resize
    ((wand (:pointer wand))
     (width :size-t)
     (height :size-t))
  :result-type :boolean)

(defun resize-image (input &key output size)
  (destructuring-bind (width height)
      (cond
        ((stringp size)
         (mapcar 'parse-integer (str:split "x" size)))
        (t size))
    (with-wand (wand :file input)
      (let ((old-width (magick-get-image-width wand))
            (old-height (magick-get-image-height wand)))
       (cond
         ((and
           (string-equal (get-image-format wand) "webp")
           (> width old-width)
           (> height old-height))
          ;; The image is already an appropriate size and format, just
          ;; hardlink it
          (if (path:-e output)
              (delete-file output))
          (copy-file-fast input output))
         (t
          ;; We really have to do the resize...
          (let ((scale (min (/ width old-width)
                            (/ height old-height)
                            1)))
           (check-boolean
            (screenshotbot-resize
             wand
             (ceiling (* scale old-width))
             (ceiling (* scale old-height)))
            wand))
          (save-as-webp wand output
                        :lossless nil)))))))


(defun magick-identify-image (wand)
  (let ((ptr (%magick-identify-image wand)))
    (unwind-protect
         (fli:convert-from-foreign-string ptr)
      (magick-relinquish-memory ptr))))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/image
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/image)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:lparallel
                #:delay
                #:future
                #:chain
                #:promise
                #:force)
  (:import-from #:hunchentoot
                #:handle-static-file)
  (:import-from #:screenshotbot/model/image
                #:no-image-uploaded-yet
                #:find-image-by-oid
                #:with-local-image)
  (:import-from #:util/object-id
                #:oid)
  (:import-from #:util/hash-lock
                #:with-hash-lock-held
                #:hash-lock)
  (:import-from #:screenshotbot/async
                #:magick-future)
  (:import-from #:util/threading
                #:with-extras
                #:ignore-and-log-errors)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:screenshotbot/magick/magick-lw
                #:screenshotbot-resize
                #:magick-crop-image
                #:save-wand-to-file
                #:magick-write-image
                #:with-wand
                #:resize-image)
  (:import-from #:util/store
                #:location-for-oid)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/cdn
                #:make-image-cdn-url)
  (:import-from #:util/events
                #:with-tracing)
  (:import-from #:util/throttler
                #:throttler
                #:throttle!
                #:ip-throttler)
  (:export
   #:handle-resized-image))
(in-package :screenshotbot/dashboard/image)

(defvar *image-resize-lock* (make-instance 'hash-lock
                                           :test 'equal))

(defvar *semaphore* nil)

#+lispworks
(defun semaphore ()
  (util:or-setf
   *semaphore*
   (mp:make-semaphore :count (serapeum:count-cpus))
   :thread-safe t))

#-lispworks
(def-easy-macro with-semaphore (&fn fn)
  (fn))

#+lispworks
(def-easy-macro with-semaphore (&fn fn)
  (let ((sem (semaphore)))
    (case (mp:semaphore-acquire sem :timeout 60)
      (nil
       (error "could not get semaphore in time"))
      (t
       (unwind-protect
            (fn)
         (mp:semaphore-release sem))))))

(defun cache-dir ()
  (let ((dir (path:catdir util/store:*object-store* "image-cache/")))
    (ensure-directories-exist dir)
    dir))

(defun %ignore (x)
  (declare (ignore x)))

(defun pathname-for-resized-image (oid &key type size
                                         (version :new))
  (ecase version
    (:old
     ;; Removing this code requires *min-store-version* to be >= 6!
     (make-pathname
      :type type
      :defaults (cache-dir)
      :name (format nil "~a-~a" oid size)))
    (:new
     (location-for-oid
      #P"image-cache/"
      (ironclad:hex-string-to-byte-array oid)
      :suffix size
      :type type))))

(defparameter *size-map*
  `(("small" . "300x300")
    ("half-page" . "600x600")
    ("full-page" . "2000x2000")
    ;; For testing only:
    ("tiny" . "5x5")))

(defun %build-resized-image (image size-name &key (type :webp))
  "Synchronous version. Do not call directly."
  (let ((size (or
               (assoc-value *size-map* size-name :test #'string-equal)
               (error "invalid image size: ~a" size-name))))
    (flet ((output-file (type)
             (pathname-for-resized-image (oid image) :type type :size size))
           (respond (res)
             res))
      (ecase type
        (:png
         (let ((webp (%build-resized-image
                      image size-name
                      :type :webp)))
           (let ((png (output-file "png")))
             (unless (uiop:file-exists-p png)
               (warn "Requesting a png that does not exist")
               (with-wand (wand :file webp)
                 (uiop:with-staging-pathname (png)
                   (save-wand-to-file
                    wand png))))
             (respond png))))
        (:webp
         (let* ((output-file (output-file "webp")))
           (cond
             ((uiop:file-exists-p output-file)
              (respond output-file))
             (t
              (unless (uiop:file-exists-p output-file)
                (with-local-image (input image)
                  (uiop:with-staging-pathname (output-file)
                    (resize-image input
                                  :output output-file
                                  :size size))))
              (respond output-file)))))))))

(defun handle-resized-image (image size &key warmup
                                          type)
  (with-extras (("image" image))
   (with-hash-lock-held ((list image size type) *image-resize-lock*)
     (cond
       (warmup
        (with-semaphore ()
          (with-tracing (:image-warmup-resize)
            (%build-resized-image image size))))
       (t
        (let ((output-file
                (with-semaphore ()
                  (%build-resized-image
                   image size
                   :type (cond
                           ((string= type "png")
                            :png)
                           (t
                            :webp))))))
          (handle-static-file
           output-file
           (format nil "image/~a" (pathname-type output-file)))))))))

(defun send-404 (reason)
  (setf (hunchentoot:header-out :content-type) "text/html")
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (hunchentoot:abort-request-handler reason))

(def-easy-macro with-access-checked-image (&binding image oid &fn fn)
  (handler-case
      (let ((oid (encrypt:decrypt-mongoid oid)))
        (assert oid)
        (let* ((image (find-image-by-oid oid)))
          (fn image)))
    (no-image-uploaded-yet ()
      (send-404 "No image uploaded yet for this image"))))

(defun set-cors-header ()
  (setf (hunchentoot:header-out :access-control-allow-origin)  "*"))

(defhandler (image-blob-get :uri "/image/blob/:oid/default.webp") (oid size type)
  (with-access-checked-image (image oid)
    (setf (hunchentoot:header-out :content-type) "image/png")
    (set-cors-header)
    (cond
      (size
       (handle-resized-image image size :type type))
      (t
       (with-local-image (file image)
         (handle-static-file file))))))

(defvar *resize-throttler* (make-instance 'ip-throttler
                                          :tokens 100
                                          :period 120))

(defvar *resize-per-company-throttler* (make-instance 'throttler
                                                      :tokens 1000))

(def-easy-macro with-cropped-and-resized (image x0 y0 w0 h0 z &key &binding output &fn fn)
  (throttle! *resize-throttler*)

  ;; Sanity check, even though MagickWand should check this too.
  (assert (< (* z w0) 16000))
  (assert (< (* z h0) 16000))

  (throttle! *resize-per-company-throttler* :key (screenshotbot/model/company:company image))

  (uiop:with-temporary-file (:pathname p :type "webp")
    (with-local-image (file image)
      (with-semaphore ()
        (with-wand (wand :file file)
          ;; TODO: sanity check input?
          (unless (magick-crop-image wand
                                     w0
                                     h0
                                     x0
                                     y0)
            (error "Could not crop image"))
          (unless (screenshotbot-resize wand
                                        (ceiling (* z w0))
                                        (ceiling (* z h0)))
            (error "Could not resize image"))
          (save-wand-to-file wand p))))
    (fn p)))

(defhandler (image-resized-blob :uri "/image/resized.webp")
    (eoid (x0 :parameter-type 'integer)
          (y0 :parameter-type 'integer)
          (w0 :parameter-type 'integer)
          (h0 :parameter-type 'integer)
          z)
  "Given an image (EOID), an initial coordate [x0 y0] and a height and
 width [w0 h0], return the region corresponding to that [x0 y0] and
 [w0 h0] zoomed to a scale z.

It is expected that the client will call this via a CDN, and will be
careful about not making multiple requests. One such strategy will be
that if we want the image for [x0 y0 w0 h0], we would instead request
the image for [x0-(x0 % 2w0) y0 - (y0 % 2h0) 2w0 2h0] and retrieve the
right region within that.
"

  (let ((z (#+lispworks hcl:parse-float #-lispworks parse-float:parse-float z)))
    (with-access-checked-image (image eoid)
     (with-cropped-and-resized (image x0 y0 w0 h0 z :output p)
       (setf (hunchentoot:header-out :content-type) "image/webp")
       (handle-static-file p)))))



(defhandler (image-blob-get-original :uri "/image/original/:eoid") (eoid)
  (destructuring-bind (oid &optional type) (str:split "." eoid)
    (with-access-checked-image (image oid)
      (with-local-image (file image)
        (set-cors-header)
        (cond
          (type
           (assert (str:s-member '("png" "webp" "jpeg")
                                 type))
           (setf (hunchentoot:header-out :content-type) (format nil "image/~a"  type))
           (handle-static-file file))
          (t
           ;; No type argument provided, let's figure it out and redirect
           (hex:safe-redirect
            (make-image-cdn-url
             (hex:make-url
              'image-blob-get-original
              :eoid (format nil "~a.~a"
                            eoid
                            (str:downcase
                             (image-format image))))))))))))

(def-store-migration ("Move image-cache to nested directories" :version 6)
  (dolist (image (bknr.datastore:class-instances 'image))
    (dolist (size (mapcar #'cdr *size-map*))
      (dolist (type (list "webp" "png"))
        (let* ((args (list (oid image) :size size :type type))
               (old (apply #'pathname-for-resized-image (append
                                                         args
                                                         (list :version :old))))
               (new (apply #'pathname-for-resized-image args)))
          (when (and
                 (path:-e old)
                 (not (path:-e new)))
            (log:info "Renaming ~a to ~a" old new)
            (rename-file old new)))))))

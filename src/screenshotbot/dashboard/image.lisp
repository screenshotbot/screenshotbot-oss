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
                #:ignore-and-log-errors)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:screenshotbot/magick/magick-lw
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
  (with-hash-lock-held ((list image size type) *image-resize-lock*)
    (cond
      (warmup
       (with-semaphore ()
        (%build-resized-image image size)))
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
          (format nil "image/~a" (pathname-type output-file))))))))

(defun send-404 (reason)
  (setf (hunchentoot:header-out :content-type) "text/html")
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (hunchentoot:abort-request-handler reason))

(defhandler (image-blob-get :uri "/image/blob/:oid/default.webp") (oid size type)
  (handler-case
      (let ((oid (encrypt:decrypt-mongoid oid)))
        (assert oid)
        (let* ((image (find-image-by-oid oid)))
          (setf (hunchentoot:header-out :content-type) "image/png")
          (cond
            (size
             (handle-resized-image image size :type type))
            (t
             (with-local-image (file image)
               (handle-static-file file))))))
    (no-image-uploaded-yet ()
      (send-404 "No image uploaded yet for this image"))))

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

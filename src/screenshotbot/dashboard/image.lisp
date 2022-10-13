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
  (:import-from #:util #:find-by-oid)
  (:import-from #:hunchentoot
                #:handle-static-file)
  (:import-from #:screenshotbot/model/image
                #:with-local-image)
  (:import-from #:util/object-id
                #:oid)
  (:import-from #:screenshotbot/magick
                #:run-magick)
  (:import-from #:util/hash-lock
                #:hash-locked-future
                #:with-hash-lock-held
                #:hash-lock)
  (:import-from #:screenshotbot/async
                #:with-magick-kernel
                #:with-screenshotbot-kernel)
  (:export
   #:handle-resized-image))
(in-package :screenshotbot/dashboard/image)

(defvar *image-resize-lock* (make-instance 'hash-lock
                                            :test 'equal))

(defun cache-dir ()
  (let ((dir (path:catdir util/store:*object-store* "image-cache/")))
    (ensure-directories-exist dir)
    dir))

(defun %ignore (x)
  (declare (ignore x)))

(defun %build-resized-image (image size-name &key type)
  "Synchronous version. Do not call directly."
  (let ((size (cond
                ((string-equal "small" size-name) "300x300")
                ((string-equal "half-page" size-name) "600x600")
                ((string-equal "full-page" size-name) "2000x2000")
                ((string-equal "tiny" size-name) "5x5") ;; for testing only
                (t (error "invalid image size: ~a" size-name)))))
    (flet ((output-file (type)
             (make-pathname
              :type type
              :defaults (cache-dir)
              :name (format nil "~a-~a" (oid image) size)))
           (respond (res)
             res))
      (ecase type
        (:png
         (warn "Requesting a png")
         (let ((webp (%build-resized-image
                      image size-name
                      :type :webp)))
           (let ((png (output-file "png")))
             (unless (uiop:file-exists-p png)
               (uiop:with-staging-pathname (png)
                 (run-magick
                  (list "convert"
                        webp
                        "-strip"
                        png))))
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
                    (run-magick
                     (list "convert" input
                           "-resize"
                           (format nil "~a>" size)
                           "-strip"
                           output-file)))))
              (respond output-file)))))))))

(defun build-resized-image (image size-name &key (type :webp))
  (with-magick-kernel ()
    (hash-locked-future ((list image size-name) *image-resize-lock*)
      (%build-resized-image image size-name
                            :type type))))

(defun handle-resized-image (image size &key warmup
                                          type)
  (cond
    (warmup
     (force
      (build-resized-image image size)))
    (t
     (let ((output-file
             (force
              (build-resized-image
               image size
               :type (cond
                       ((string= type "webp")
                        :webp)
                       ((string= type "png")
                        :png)
                       (t
                        :png))))))
       (handle-static-file
        output-file
        (format nil "image/~a" (pathname-type output-file)))))))

(defhandler (image-blob-get :uri "/image/blob/:oid/default.webp") (oid size type)
  (let ((oid (encrypt:decrypt-mongoid oid)))
    (assert oid)
    (let* ((image (find-by-oid oid)))
      (check-type image image)
      (setf (hunchentoot:header-out :content-type) "image/png")
      (cond
        (size
         (handle-resized-image image size :type type))
        (t
         (with-local-image (file image)
           (handle-static-file file)))))))

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
                #:with-hash-lock-held
                #:hash-lock)
  (:export
   #:handle-resized-image))
(in-package :screenshotbot/dashboard/image)

(defvar *image-resize-lock* (make-instance 'hash-lock
                                            :test 'equal))

(defun cache-dir ()
  (let ((dir (path:catdir util/store:*object-store* "image-cache/")))
    (ensure-directories-exist dir)
    dir))

(defun build-resized-image (image size-name &key (type :webp))
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
             :name (format nil "~a-~a" (oid image) size))))
     (ecase type
       (:png
        (warn "Requesting a png")
        (let ((webp (build-resized-image
                     image size-name
                     :type :webp))
              (png (output-file "png")))
          (with-hash-lock-held ((list image size) *image-resize-lock*)
            (unless (uiop:file-exists-p png)
              (uiop:with-staging-pathname (png)
                (run-magick
                 (list "convert"
                       "-limit" "memory" "3MB"
                       "-limit" "disk" "500MB"
                       webp
                       "-strip"
                       png)))))
          png))
       (:webp
        (let* ((output-file (output-file "webp")))
          (cond
            ((uiop:file-exists-p output-file)
             output-file)
            (t
             (with-hash-lock-held ((list image size) *image-resize-lock*)
               (unless (uiop:file-exists-p output-file)
                 (with-local-image (input image)
                   (uiop:with-staging-pathname (output-file)
                     (run-magick
                      (list "convert" input
                            "-limit" "memory" "3MB"
                            "-limit" "disk" "500MB"
                            "-resize"
                            (format nil "~a>" size)
                            "-strip"
                            output-file))))))
             output-file))))))))

(defun webp-supported-p (user-agent accept)
  (or
   (str:containsp "image/webp" accept)
   (str:starts-with-p "curl" user-agent)))

(defun handle-resized-image (image size &key warmup)
  (cond
    (warmup
     (build-resized-image image size))
    (t
     (let ((output-file (build-resized-image
                         image size
                         :type (if (webp-supported-p
                                    (hunchentoot:header-in* :user-agent)
                                    (hunchentoot:header-in* :accept))
                                   :webp :png))))
       (handle-static-file
        output-file
        (format nil "image/~a" (pathname-type output-file)))))))

(defhandler (image-blob-get :uri "/image/blob/:oid/default.webp") (oid size)
  (let* ((image (find-by-oid oid)))
    (setf (hunchentoot:header-out :content-type) "image/png")

    (cond
      (size
       (handle-resized-image image size))
      (t
       (with-local-image (file image)
        (handle-static-file file))))))

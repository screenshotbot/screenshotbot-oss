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
                #:run-magick))
(in-package :screenshotbot/dashboard/image)

(defvar *lock* (bt:make-lock "image-resize"))

(defvar *image-resize-locks* (make-hash-table)
  "A hash table of locks, one for each image object. Sure, they're
  never garbage collected, but it should never be large enough to
  cause an issue.")

(defun resize-lock (image)
  (bt:with-lock-held (*lock*)
    (util:or-setf
     (gethash image *image-resize-locks*)
     (bt:make-lock "image-resize-lock"))))

(defun cache-dir ()
  (let ((dir (path:catdir util/store:*object-store* "image-cache/")))
    (ensure-directories-exist dir)
    dir))

(defun handle-resized-image (image size)
  (let* ((size (cond
                 ((string-equal "small" size) "300x300")
                 ((string-equal "half-page" size) "600x600")
                 ((string-equal "full-page" size) "2000x2000")
                (t (error "invalid image size: ~a" size))))
         (output-file
           (make-pathname
            :type "png"
            :defaults (cache-dir)
            :name (format nil "~a-~a" (oid image) size))))
    (cond
      ((uiop:file-exists-p output-file)
       (handle-static-file output-file))
      (t
       (bt:with-lock-held ((resize-lock image))
         (unless (uiop:file-exists-p output-file)
           (with-local-image (input image)
             (uiop:with-staging-pathname (output-file)
               (run-magick
                (list "convert" input
                      "-limit" "memory" "3MB"
                      "-limit" "disk" "500MB"
                      "-adaptive-resize"
                      (format nil "~a>" size)
                      output-file))))))
       (handle-static-file output-file)))))

(defhandler (image-blob-get :uri "/image/blob/:oid/default.png") (oid size)
  (let* ((image (find-by-oid oid))
         (blob (image-blob image)))
    (setf (hunchentoot:header-out :content-type) "image/png")

    (cond
      (size
       (handle-resized-image image size))
      (t
       (handle-static-file (bknr.datastore:blob-pathname blob))))))

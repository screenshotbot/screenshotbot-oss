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

(defun handle-resized-image (image size &key warmup)
  (let* ((size (cond
                 ((string-equal "small" size) "300x300")
                 ((string-equal "half-page" size) "600x600")
                 ((string-equal "full-page" size) "2000x2000")
                 ((string-equal "tiny" size) "5x5") ;; for testing only
                 (t (error "invalid image size: ~a" size))))
         (output-file
           (make-pathname
            :type "png"
            :defaults (cache-dir)
            :name (format nil "~a-~a" (oid image) size))))
    (flet ((finish ()
             (cond
               (warmup
                output-file)
               (t
                (handle-static-file output-file)))))
      (cond
       ((uiop:file-exists-p output-file)
        (finish))
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
                       output-file))))))
        (finish))))))

(defhandler (image-blob-get :uri "/image/blob/:oid/default.png") (oid size)
  (let* ((image (find-by-oid oid)))
    (setf (hunchentoot:header-out :content-type) "image/png")

    (cond
      (size
       (handle-resized-image image size))
      (t
       (with-local-image (file image)
        (handle-static-file file))))))

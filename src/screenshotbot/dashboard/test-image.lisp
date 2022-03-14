;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-image
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/image
                #:handle-resized-image)
  (:import-from #:screenshotbot/model
                #:image-blob)
  (:import-from #:screenshotbot/model/image
                #:image)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-image)


(util/fiveam:def-suite)

(test handle-resized-image-warmup-happy-path
  (tmpdir:with-tmpdir (util/store:*object-store*)
   (let ((objs))
     (flet ((save (x)
              (push x objs)
              x))
       (unwind-protect
            (let* ((im1 (asdf:system-relative-pathname
                         :screenshotbot
                         "dashboard/fixture/image.png"))
                   (im-blob (save (make-instance 'image-blob)))
                   (im (save (make-instance 'image
                                             :blob im-blob))))
              (uiop:copy-file im1 (blob-pathname im-blob))
              (is (uiop:file-exists-p im1))
              (let ((output-file (handle-resized-image im :tiny :warmup t)))
                (unwind-protect
                     (is (equal output-file
                                (handle-resized-image im :tiny :warmup t)))
                     (uiop:file-exists-p output-file)
                  (delete-file output-file))))
         (loop for x in objs do
           (delete-object x)))))))

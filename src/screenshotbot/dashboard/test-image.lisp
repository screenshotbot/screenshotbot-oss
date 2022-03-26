;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-image
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/image
                #:build-resized-image
                #:handle-resized-image)
  (:import-from #:screenshotbot/model
                #:image-blob)
  (:import-from #:screenshotbot/model/image
                #:image)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:util/store
                #:with-test-store)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-image)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (tmpdir:with-tmpdir (util/store:*object-store*)
      (let* ((im1 (asdf:system-relative-pathname
                :screenshotbot
                "dashboard/fixture/image.png"))
          (im-blob (make-instance 'image-blob))
          (im (make-instance 'image
                              :blob im-blob)))
        (uiop:copy-file im1 (blob-pathname im-blob))
        (&body)))))

(test handle-resized-image-warmup-happy-path
  (with-fixture state ()
    (is (uiop:file-exists-p im1))
    (let ((output-file (handle-resized-image im :tiny :warmup t)))
      (unwind-protect
           (progn
             (is (equal "webp" (pathname-type output-file)))
             (is (equal output-file
                        (handle-resized-image im :tiny :warmup t))))
        (uiop:file-exists-p output-file)))))

(test build-resized-image-for-png
  (with-fixture state ()
    (let ((output-file (build-resized-image im :tiny :type :png)))
      (is (equal "png" (pathname-type output-file)))
      (build-resized-image im :tiny :type :png))))

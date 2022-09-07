;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-image-comparison
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/model/image-comparison
                #:image-comparison
                #:do-image-comparison)
  (:import-from #:screenshotbot/model/image
                #:image-blob)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-image-comparison)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (tmpdir:with-tmpdir (dir)
     (with-fake-request ()
       (let ((im1 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
             (im2 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-2.png"))
             (im3 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-3.png"))
             (objs))
         (labels ((make-screenshot (img)
                    (let* ((image-blob (make-instance 'image-blob))
                           (image (make-instance 'screenshotbot/model:image
                                                  :blob image-blob)))
                      (uiop:copy-file img (blob-pathname image-blob))
                      (make-instance 'screenshot
                                      :name "foobar"
                                      :image image))))
           (&body)))))))

(test do-image-comparison
  (with-fixture state ()
    (is-true (uiop:file-exists-p im1))
    (is-true (uiop:file-exists-p im2))
    (uiop:with-temporary-file (:pathname out :type "png")
      (is-false
       (do-image-comparison (make-screenshot im1) (make-screenshot im3) out nil)))
    (uiop:with-temporary-file (:pathname out :type "png")
      (is-true
       (do-image-comparison (make-screenshot im1) (make-screenshot im2) out nil)))))

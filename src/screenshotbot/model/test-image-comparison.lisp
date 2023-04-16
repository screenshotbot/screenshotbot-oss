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
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/model/image-comparison
                #:ensure-db
                #:*db*
                #:image-comparison
                #:do-image-comparison)
  (:import-from #:screenshotbot/model/image
                #:with-local-image
                #:mask-rect
                #:make-image
                #:image-blob)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot)
  (:import-from #:screenshotbot/model/image-comparison
                #:%image-comparisons-for-before
                #:find-image-comparison-on-images)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/magick/magick-lw
                #:magick-write-image
                #:magick-read-image
                #:check-boolean
                #:magick-set-size
                #:with-wand)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#:fli #:util/fake-fli)))
(in-package :screenshotbot/model/test-image-comparison)

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*installation* (make-instance 'installation)))
   (with-test-store ()
     (tmpdir:with-tmpdir (dir)
       (with-fake-request ()
         (let ((im1 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
               (im2 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-2.png"))
               (im3 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-3.png"))
               (objs))
           (labels ((make-screenshot (img)
                      (let* ((image (make-image :pathname img)))
                        (make-instance 'screenshot
                                        :name "foobar"
                                        :image image))))
             (&body))))))))

(test ensure-db-multiple-times
  (with-fixture state ()
    (finishes (ensure-db)))
  (is (eql nil *db*))
  (with-fixture state ()
    (finishes
     (ensure-db))))

(test do-image-comparison
  (with-fixture state ()
    (is-true (uiop:file-exists-p im1))
    (is-true (uiop:file-exists-p im2))
    (uiop:with-temporary-file (:pathname out :type "png")
      (is-false
       (do-image-comparison (make-screenshot im1) (make-screenshot im3) out)))
    (uiop:with-temporary-file (:pathname out :type "png")
      (is-true
       (do-image-comparison (make-screenshot im1) (make-screenshot im2) out)))))

(test do-image-comparison-for-webp
  (with-fixture state ()
    (is-true (uiop:file-exists-p im1))
    (is-true (uiop:file-exists-p im2))
    (uiop:with-temporary-file (:pathname out :type "webp")
      (finishes
        (do-image-comparison (make-screenshot im1) (make-screenshot im3) out)))))

(test find-image-comparison-on-images
  (with-fixture state ()
    (let ((before (make-image :pathname im1))
          (after (make-image :pathname im2)))
      ;; this will create a new image-comparison
      (let ((result (find-image-comparison-on-images before after)))
        (is-true result)
        ;; We used to test that this object is the same as before, but
        ;; with sqlite that might no longer be the case.
        (is-true (find-image-comparison-on-images before after))))))


(def-easy-macro with-file-copy (&binding result file &fn fn)
  (uiop:with-temporary-file (:pathname res :stream s)
    (close s)
    (delete-file res)
    (fad:copy-file file res)
    (funcall fn (namestring res))))

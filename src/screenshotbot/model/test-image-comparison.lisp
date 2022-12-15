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
                #:find-old-image-comparisons
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
                #:transient-objects-for-before
                #:make-transient
                #:%image-comparisons-for-before
                #:find-existing-image-comparison
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

(test do-image-comparison-with-masks
  (with-fixture state ()
    (is-true (uiop:file-exists-p im1))
    (is-true (uiop:file-exists-p im2))
    (uiop:with-temporary-file (:pathname out :type "webp")
      (finishes
        (do-image-comparison (make-screenshot im1) (make-screenshot im3) out
          (list
           (make-instance
            'mask-rect
            :top 3
            :left 3
            :width 10
            :height 10)))))))

(test find-image-comparison-on-images
  (with-fixture state ()
    (let ((before (make-image :pathname im1))
          (after (make-image :pathname im2)))
      ;; this will create a new image-comparison
      (let ((result (find-image-comparison-on-images before after nil)))
        (is-true result)
        (is (eql result (find-image-comparison-on-images before after nil)))))))


(test find-existing-image-comparison
  (with-fixture state ()
    (let ((before (make-image :pathname im1))
          (after (make-image :pathname im2)))
      (is (null (find-existing-image-comparison before after nil)))
      (let ((expected (make-instance 'image-comparison
                                      :before before
                                      :after after
                                      :masks nil)))
        (is (eql expected (find-existing-image-comparison
                           before after nil)))))))

(test find-existing-image-comparison-transient
  (with-fixture state ()
    (let ((before (make-image :pathname im1))
          (after (make-image :pathname im2)))
      (is (null (find-existing-image-comparison before after nil)))
      (let ((expected (make-instance 'image-comparison
                                      :before before
                                      :after after
                                      :result (make-image :pathname im3)
                                      :identical-p t
                                      :masks (list
                                              (make-instance 'mask-rect
                                                             :left 0
                                                             :top 0
                                                             :width 10
                                                             :height 10)))))
        (is (equal 1 (length (%image-comparisons-for-before before))))
        (make-transient expected)
        (is (equal 1 (length (transient-objects-for-before before))))
        (is (equal 0 (length (%image-comparisons-for-before before))))))))

(def-easy-macro with-file-copy (&binding result file &fn fn)
  (uiop:with-temporary-file (:pathname res :stream s)
    (close s)
    (delete-file res)
    (fad:copy-file file res)
    (funcall fn (namestring res))))

#+lispworks
(fli:define-c-struct timeval
    (sec  :long
          :initarg :sec)
  (usec :long
        :initarg :usec))

#+lispworks
(fli:define-foreign-function utimes
    ((filename (:reference-pass :ef-mb-string))
     (times (:c-array timeval 2)))
  :result-type :int)

#+(and :lispworks (not :mswindows))
(test find-old-image-comparisons
  (with-fixture state ()
    (fli:with-dynamic-foreign-objects ((times timeval :nelems 2 :fill 0))
      (with-file-copy (file-1 im1)
        (with-file-copy (file-2 im2)
          (let* ((im-1 (make-image :pathname file-1))
                 (cmp-1 (make-instance 'image-comparison
                                       :result im-1))
                 (cmp-2 (make-instance 'image-comparison
                                       :result (make-image :pathname file-2))))
            (with-local-image (pathname im-1)
              (utimes (namestring pathname) times))
            (is (equal (list cmp-1)
                       (find-old-image-comparisons)))
            (pass)))))))

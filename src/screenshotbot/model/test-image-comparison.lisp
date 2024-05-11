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
                #:image-comparison-after
                #:image-comparison-result
                #:find-image-comparison-from-cache
                #:image-comparison-before
                #:make-image-comparison
                #:*stored-cache*
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
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#:fli #:util/fake-fli)))
(in-package :screenshotbot/model/test-image-comparison)

(util/fiveam:def-suite)

(defvar im1 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
(defvar im2 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-2.png"))
(defvar im3 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-3.png"))

(def-fixture state (&key dir)
  (let ((*installation* (make-instance 'installation)))
    (with-test-store ()
      (flet ((inner (dir)
               (with-fake-request ()
                 (let (objs)
                   (labels ((make-screenshot (img)
                              (let* ((image (make-image :pathname img)))
                                (make-instance 'screenshot
                                               :name "foobar"
                                               :image image))))
                     (&body))))))
        (cond
          (dir
           (inner dir))
          (t
           (tmpdir:with-tmpdir (dir2)
             (inner dir2))))))))

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

(test propagates-company
  (with-fixture state ()
    (let ((before (make-image :pathname im1 :company :company-1))
          (after (make-image :pathname im2 :company :company-2)))
      (let ((result (find-image-comparison-on-images before after)))
        (is
         (eql
          :company-2
          (company (image-comparison-result result)))))))
  ;; What if we swap the before and after? As implemented now, order
  ;; matters.
  (with-fixture state ()
    (let ((before (make-image :pathname im1 :company :company-1))
          (after (make-image :pathname im2 :company :company-2)))
      (let ((result (find-image-comparison-on-images after before #| here |#)))
        (is
         (eql
          :company-1
          (company (image-comparison-result result))))))))


(def-easy-macro with-file-copy (&binding result file &fn fn)
  (uiop:with-temporary-file (:pathname res :stream s)
    (close s)
    (delete-file res)
    (fad:copy-file file res)
    (funcall fn (namestring res))))

(def-fixture stored-cache ()
  (unwind-protect
       (&body)
    (setf *stored-cache* (fset:empty-set))))

(test saving-and-restoring-subsystem
  (with-fixture stored-cache ()
    (is (fset:equal? *stored-cache* (fset:empty-set)))
    (tmpdir:with-tmpdir (dir)
      (with-test-store (:dir dir)
        (let ((im1 (make-image-comparison :before "foo" :after "bar"
                                          :result "car"))
              (im2 (make-image-comparison :before "foo1" :after "bar1"
                                          :result "car1")))
          (util:safe-snapshot)
          (is (eql 2 (fset:size *stored-cache*)))))
      (is (fset:equal? *stored-cache* (fset:empty-set)))
      (with-test-store (:dir dir)
        (is (eql 2 (fset:size *stored-cache*)))
        (is (equal "foo"
                   (image-comparison-before (fset:least *stored-cache*))))
        (is (equal "foo1"
                   (image-comparison-before (fset:greatest *stored-cache*))))))))

(defun check-for-bad-state ()
  #+lispworks ;; For reasons I don't yet understand, this failed on SBCL
  (fset:do-set (imc *stored-cache*)
    (is-true (image-comparison-before imc))
    (is-true (image-comparison-after imc))
    (is-true (image-comparison-result imc))))

(test saving-and-restoring-actual-image-objects
  (with-fixture stored-cache ()
    (let ((*installation* (make-instance 'installation)))
     (tmpdir:with-tmpdir (dir)
       (with-test-store (:dir dir)
         (let ((s1 (make-image :pathname im1))
               (s2 (make-image :pathname im2)))
           (make-image-comparison :before s1 :after s2 :result s2)
           (is (eql 1 (fset:size *stored-cache*)))
           (check-for-bad-state)
           (util:safe-snapshot)))
       (is (eql 0 (fset:size *stored-cache*)))
       (with-test-store (:dir dir)
         (is (eql 1 (fset:size *stored-cache*)))
         (check-for-bad-state))))))

(test finding-image-comparisons
  (with-fixture stored-cache ()
    (let ((*installation* (make-instance 'installation)))
     (tmpdir:with-tmpdir (dir)
       (with-test-store (:dir dir)
         (let ((s1 (make-image :pathname im1))
               (s2 (make-image :pathname im2)))
           (make-image-comparison :before s1 :after s2 :result "bleh")
           (let ((imc
                   (find-image-comparison-from-cache
                    :before s1 :after s2)))
             (is (equal "bleh" (image-comparison-result imc))))))))))

(test saving-deleted-objects
  (with-fixture stored-cache ()
    (let ((*installation* (make-instance 'installation)))
     (tmpdir:with-tmpdir (dir)
       (with-test-store (:dir dir)
         (let ((s1 (make-image :pathname im1))
               (s2 (make-image :pathname im2)))
           (make-image-comparison :before s1 :after s2 :result s2)
           (make-image-comparison :before s2 :after s2 :result s2)
           (is (eql 2 (fset:size *stored-cache*)))
           (check-for-bad-state)
           (delete-object s1)
           (util:safe-snapshot)))
       (is (eql 0 (fset:size *stored-cache*)))
       (with-test-store (:dir dir)
         (is (eql 1 (fset:size *stored-cache*)))
         (check-for-bad-state))))))

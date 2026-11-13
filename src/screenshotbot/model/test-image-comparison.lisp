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
                #:remove-image-comparison
                #:image-comparison-changed-rows
                #:read-snapshot
                #:write-snapshot
                #:transient-image-comparison
                #:identical-p
                #:image-comparison-difference-value
                #:image-comparison-after
                #:image-comparison-result
                #:find-image-comparison-from-cache
                #:image-comparison-before
                #:make-image-comparison
                #:*stored-cache*
                #:image-comparison
                #:do-image-comparison
                #:find-image-comparison-on-images)
  (:import-from #:screenshotbot/model/image
                #:with-local-image
                #:make-image)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/magick/magick-lw
                #:magick-get-image-height
                #:magick-get-image-width
                #:with-wand)
  (:import-from #:bknr.datastore
                #:encode
                #:delete-object)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/testing
                #:with-test-image)
  (:import-from #:screenshotbot/magick/magick-lw
                #:foreign-function-defined-p)
  (:import-from #:cl-mock
                #:if-called
                #:with-mocks)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#:fli #:util/fake-fli)))
(in-package :screenshotbot/model/test-image-comparison)

(util/fiveam:def-suite)

;; TODO: im1 and im2 seem like they're identical, and that might've been a typo from the past
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
          (after (make-image :pathname im3)))
      ;; this will create a new image-comparison
      (let ((result (find-image-comparison-on-images before after)))
        (is-true result)
        ;; We used to test that this object is the same as before, but
        ;; with sqlite that might no longer be the case.
        (is-true (find-image-comparison-on-images before after))
        (is-false (identical-p result))
        (is (< 0 (image-comparison-difference-value result)))))))

(test changed-rows-is-not-created-without-the-gk
  (with-fixture state ()
    ;; The gk is enabled by default, so explicitly create it as disabled.
    (gk:create :image-strip-diff)
    (let ((before (make-image :pathname im1))
          (after (make-image :pathname im3)))
      (let ((result (find-image-comparison-on-images before after)))
        (is-false (identical-p result))
        (is (equal nil (image-comparison-changed-rows result)))))))

(def-fixture image-strip-diff-enabled ()
  (let ((company (make-instance 'company)))
    (gk:create :image-strip-diff)
    (gk:allow :image-strip-diff company)
    (let ((before (make-image :pathname im1 :company company))
          (after (make-image :pathname im3 :company company)))
      (&body))))

(test changed-rows-is-created-with-the-gk
  (with-fixture state ()
    (with-fixture image-strip-diff-enabled ()
      (let* ((result (find-image-comparison-on-images before after))
             (changed-rows (image-comparison-changed-rows result)))
        (is-true changed-rows)
        (with-local-image (result-file (image-comparison-result result))
          (with-local-image (file changed-rows)
            (is (equal (list 1 (second (image-dimensions-for-pathname result-file)))
                       (image-dimensions-for-pathname file)))))))))

(test changed-rows-is-skipped-if-magick-native-is-too-old
  (with-fixture state ()
    (with-fixture image-strip-diff-enabled ()
      (with-mocks ()
        (if-called 'foreign-function-defined-p
                   (lambda (name)
                     (declare (ignore name))
                     nil))
        (let ((result (handler-bind ((warning #'muffle-warning))
                        (find-image-comparison-on-images before after))))
          (is-true (image-comparison-result result))
          (is-false (identical-p result))
          (is (equal nil (image-comparison-changed-rows result))))))))

(test changed-rows-is-backfilled-along-with-difference-value
  (with-fixture state ()
    (with-fixture image-strip-diff-enabled ()
      (let ((result (find-image-comparison-on-images before after)))
        (setf (image-comparison-difference-value result) nil)
        (setf (image-comparison-changed-rows result) nil)
        (find-image-comparison-on-images before after)
        (is-true (image-comparison-difference-value result))
        (is-true (image-comparison-changed-rows result))))))

(test difference-value-is-populated-if-it-doesnt-exist
  "This is a migration strategy, to avoid having to recompute everything
from beginning of time. It might be reasonable to delete in the future."
  (with-fixture state ()
    (let ((before (make-image :pathname im1))
          (after (make-image :pathname im3)))
      ;; this will create a new image-comparison
      (let ((result (find-image-comparison-on-images before after)))
        (setf (image-comparison-difference-value result) nil)

        (is-true (find-image-comparison-on-images before after))
        (is-false (identical-p result))

        (is-true (image-comparison-difference-value result))
        (is (< 0 (image-comparison-difference-value result)))))))

(test difference-value-is-not-populated-if-we-request-only-cached-values
  "Same as the previous test, if if we have :only-cached-p, then we don't expect
to be doing image processing during the request."
  (with-fixture state ()
    (let ((before (make-image :pathname im1))
          (after (make-image :pathname im3)))
      ;; this will create a new image-comparison
      (let ((result (find-image-comparison-on-images before after)))
        (setf (image-comparison-difference-value result) nil)

        (is-true (find-image-comparison-on-images before after :only-cached-p t))
        (is-false (identical-p result))

        (is-false (image-comparison-difference-value result))))))

(test order-doesnt-matter
  (with-fixture state ()
    (let ((before (make-image :pathname im1))
          (after (make-image :pathname im3)))
      ;; this will create a new image-comparison
      (let ((result (find-image-comparison-on-images before after)))
        (is-true result)
        (is (eql result (find-image-comparison-on-images after before)))))))

(test only-cached-p-should-not-create-a-new-one
  (with-fixture state ()
    (let ((before (make-image :pathname im1))
          (after (make-image :pathname im3)))
      ;; this will create a new image-comparison
      (is (eql nil
               (find-image-comparison-on-images before after
                                                :only-cached-p t))))))

(test propagates-company
  (with-fixture state ()
    (let ((before (make-image :pathname im1 :company :company-2))
          (after (make-image :pathname im2 :company :company-2)))
      (let ((result (find-image-comparison-on-images before after)))
        (is
         (eql
          :company-2
          (company (image-comparison-result result))))))))

(test both-images-have-to-be-same-company
  (with-fixture state ()
    (let ((before (make-image :pathname im1 :company :company-1))
          (after (make-image :pathname im2 :company :company-2)))
      (signals simple-error
       (find-image-comparison-on-images after before #| here |#)))))


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
                                          :result "car"
                                          :changed-rows "dar"))
              (im2 (make-image-comparison :before "foo1" :after "bar1"
                                          :result "car1")))
          (util:safe-snapshot)
          (is (eql 2 (fset:size *stored-cache*)))))
      (is (fset:equal? *stored-cache* (fset:empty-set)))
      (with-test-store (:dir dir)
        (is (eql 2 (fset:size *stored-cache*)))
        (is (equal "foo"
                   (image-comparison-before (fset:least *stored-cache*))))
        (is (equal "dar"
                   (image-comparison-changed-rows (fset:least *stored-cache*))))
        (is (equal nil
                   (image-comparison-changed-rows (fset:greatest *stored-cache*))))
        (is (equal "foo1"
                   (image-comparison-before (fset:greatest *stored-cache*))))))))

(def-easy-macro with-v1-snapshot (&binding pathname &fn fn)
  "Writes a snapshot in the version 1 format, from before CHANGED-ROWS
was added."
  (uiop:with-temporary-file (:pathname pathname :stream output
                             :element-type '(unsigned-byte 8))
    (encode 1 output)
    (encode 2 output)
    (loop for (before after result identical) in '(("foo" "bar" "car" 1)
                                                   ("foo1" "bar1" "car1" 0))
          do
             (encode before output)
             (encode after output)
             (encode result output)
             (encode identical output))
    (finish-output output)
    (fn pathname)))

(test can-read-v1-snapshot
  (with-v1-snapshot (pathname)
    (let ((objs (sort (read-snapshot pathname) #'string<
                      :key #'image-comparison-before)))
      (is (equal '("foo" "foo1") (mapcar #'image-comparison-before objs)))
      (is (equal '("bar" "bar1") (mapcar #'image-comparison-after objs)))
      (is (equal '("car" "car1") (mapcar #'image-comparison-result objs)))
      (is (equal '(t nil) (mapcar #'identical-p objs)))
      (is (equal '(nil nil) (mapcar #'image-comparison-changed-rows objs))))))

(test write-and-read-snapshot-with-changed-rows
  (uiop:with-temporary-file (:pathname pathname)
    (write-snapshot pathname
                    (fset:set
                     (make-instance 'transient-image-comparison
                                    :before "foo" :after "bar" :result "car"
                                    :identical-p t
                                    :changed-rows "dar")))
    (let ((obj (first (read-snapshot pathname))))
      (is (equal "foo" (image-comparison-before obj)))
      (is (equal "car" (image-comparison-result obj)))
      (is-true (identical-p obj))
      (is (equal "dar" (image-comparison-changed-rows obj))))))

(test deleted-changed-rows-is-written-as-nil
  (with-fixture state ()
    (let ((changed-rows (make-image :pathname im1)))
      (delete-object changed-rows)
      (uiop:with-temporary-file (:pathname pathname)
        (write-snapshot pathname
                        (fset:set
                         (make-instance 'transient-image-comparison
                                        :before "foo" :after "bar" :result "car"
                                        :changed-rows changed-rows)))
        (let ((obj (first (read-snapshot pathname))))
          (is (equal "foo" (image-comparison-before obj)))
          (is (equal nil (image-comparison-changed-rows obj))))))))

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

(defun image-dimensions-for-pathname (pathname)
  "Returns a list of (width height) for the image at pathname"
  (with-wand (wand :file pathname)
    (list (magick-get-image-width wand)
          (magick-get-image-height wand))))



(test different-sized-images-should-not-be-identical
  "Regression test: images of different sizes should never be considered identical,
   even if all overlapping pixels match"
  (with-fixture state ()
    (uiop:with-temporary-file (:pathname out :type "png")
      ;; Test case 1: second image larger than first
      (with-test-image (small-img :width 10 :height 10 :color "red")
        (with-test-image (large-img :width 20 :height 20 :color "red")
          (is-false (do-image-comparison 
                      (make-screenshot small-img) 
                      (make-screenshot large-img) 
                      out))
          (is (equal
               (list 20 20)
               (image-dimensions-for-pathname out)))))
      
      ;; Test case 2: first image larger than second  
      (with-test-image (large-img :width 20 :height 20 :color "blue")
        (with-test-image (small-img :width 10 :height 10 :color "blue")
          (is-false (do-image-comparison 
                      (make-screenshot large-img) 
                      (make-screenshot small-img) 
                      out))
          (is (equal
               (list 20 20)
               (image-dimensions-for-pathname out))))))))

(test different-sized-images-should-not-be-identical--with-only-height-change
  "Regression test: images of different sizes should never be considered identical,
   even if all overlapping pixels match"
  (with-fixture state ()
    (uiop:with-temporary-file (:pathname out :type "png")
      ;; Test case 1: second image larger than first
      (with-test-image (small-img :width 10 :height 10 :color "red")
        (with-test-image (large-img :width 10 :height 20 :color "red")
          (is-false (do-image-comparison 
                      (make-screenshot small-img) 
                      (make-screenshot large-img) 
                      out))
          (is (equal
               (list 10 20)
               (image-dimensions-for-pathname out)))))
      
      ;; Test case 2: first image larger than second  
      (with-test-image (large-img :width 10 :height 20 :color "blue")
        (with-test-image (small-img :width 10 :height 10 :color "blue")
          (is-false (do-image-comparison 
                      (make-screenshot large-img) 
                      (make-screenshot small-img) 
                      out))
          (is (equal
               (list 10 20)
               (image-dimensions-for-pathname out))))))))

(test remove-image-comparison
  "Test that we can create and remove image-comparison objects from the cache"
  (with-fixture stored-cache ()
    (let ((*installation* (make-instance 'installation)))
      (tmpdir:with-tmpdir (dir)
        (with-test-store (:dir dir)
          (let ((s1 (make-image :pathname im1))
                (s2 (make-image :pathname im2))
                (s3 (make-image :pathname im3)))
            ;; Create two image comparisons
            (let ((imc1 (make-image-comparison :before s1 :after s2 :result s3))
                  (imc2 (make-image-comparison :before s2 :after s3 :result s1)))
              ;; Verify both are in the cache
              (is (eql 2 (fset:size *stored-cache*)))
              ;; Remove one of them
              (remove-image-comparison s1 s2)
              ;; Verify only one remains
              (is (eql 1 (fset:size *stored-cache*)))
              ;; Verify the correct one remains
              (is (eql imc2 (find-image-comparison-from-cache :before s2 :after s3)))
              ;; Verify the removed one is gone
              (is (eql nil (find-image-comparison-from-cache :before s1 :after s2))))))))))


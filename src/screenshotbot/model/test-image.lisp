;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/test-image
  (:use #:cl
        #:fiveam
        #:screenshotbot/model/image)
  (:import-from #:screenshotbot/model/image
                #:delete-image
                #:image-file-deleted
                #:update-company-image-size
                #:get-company-image-size
                #:img-tmp-dir
                #:%image-size
                #:image-reuploaded-warning
                #:no-image-uploaded-yet
                #:base-image-comparer
                #:all-soft-expired-images
                #:soft-expiration-time
                #:image-filesystem-pathname
                #:image
                #:local-image
                #:image-blob
                #:%perceptual-hash
                #:image-blob-get
                #:image=)
  (:import-from #:util
                #:oid)
  (:import-from #:screenshotbot/model/image
                #:%image-state
                #:+image-state-filesystem+
                #:with-local-image
                #:update-image
                #:local-location-for-oid
                #:slow-image-comparison
                #:image-format
                #:dimension-width
                #:dimension-height
                #:dimension
                #:image-dimensions
                #:map-unequal-pixels-on-file
                #:map-unequal-pixels-arr
                #:%map-unequal-pixels
                #:transient-mask-rect
                #:map-unequal-pixels)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:util/digests
                #:md5-file)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/object-id
                #:make-oid
                #:%make-oid
                #:oid-array
                #:oid-arr)
  (:import-from #:screenshotbot/magick/magick-lw
                #:with-wand
                #:save-wand-to-file)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:export))

(util/fiveam:def-suite)

(defun %%image= (&rest args)
  "For all of these tests, we're only concerned with the version that
uses the base-image-comparer."
  (apply #'image=
         (make-instance 'base-image-comparer)
         args))

(defun static-asset (file)
  (path:catfile
     #.(asdf:system-relative-pathname :screenshotbot
                                      "static/")
     file))


(def-easy-macro with-base-fixture (&fn fn)
  (let ((*installation* (make-instance 'installation)))
    (funcall fn)))

(def-fixture state (&key dir)
  (with-base-fixture ()
   (let ((file #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png")))
     (with-test-store (:dir dir)
       (let* ((img (make-image
                    :pathname (static-asset "assets/images/old-example-view-right.png")))
              (img2 (make-image
                     :pathname (static-asset "assets/images/old-example-view-left.png")))
              (img-copy (make-image
                         :pathname (static-asset "assets/images/old-example-view-right.png")))
              (rect (make-instance 'mask-rect :left 7 :top 10
                                              :height 100 :width 104)))
         (&body))))))

(test image-comparison-is-cached ()
  (with-fixture state ()
    (let ((got-signal nil))
     (handler-bind ((slow-image-comparison
                      (lambda (e)
                        (setf got-signal t))))
       (is-true (%%image=
                 img img2 (list rect)))
       (is-true got-signal)))
    (handler-bind ((slow-image-comparison
                    (lambda (e)
                      (fail "Should not get slow-image-comparison"))))
      (is-true (%%image=
                img img2 (list rect))))))

(test image-comparison-is-cached-for-unequal ()
  (with-fixture state ()
    (let ((img (make-magick-test-image "rose:"))
          (img2 (make-magick-test-image "wizard:")))
      (let ((got-signal nil))
        (handler-bind ((slow-image-comparison
                         (lambda (e)
                           (setf got-signal t))))
          (is-false (%%image=
                     img img2 (list rect)))
          (is-true got-signal)))
      (handler-bind ((slow-image-comparison
                       (lambda (e)
                         (fail "Should not get slow-image-comparison"))))
        (is-false (%%image=
                   img img2 (list rect)))))))


(test simple-compare ()
  (with-fixture state ()
    (is-true (%%image=
              img img nil))
    (is-true (%%image= img img-copy nil))
    (is-false (%%image= img img2 nil))
    (is-true (%%image= img img2 (list rect)))))

(defun make-magick-test-image (name)
  (uiop:with-temporary-file (:pathname p :type "webp")
    (with-wand (wand :file name)
      (save-wand-to-file wand p))
    (make-test-image p)))

(defun make-test-image (pathname)
  (let* ((image (make-image
                 :pathname pathname)))
    image))

(test image-dimensions
  (with-fixture state ()
    (let* ((file #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
           (image (make-image :pathname file)))
      (let ((dimension (image-dimensions image)))
        (is (typep dimension 'dimension))
        (is (eql 360 (dimension-height dimension)))
        (is (eql 360 (dimension-width  dimension)))))))

(test image-format
  (with-fixture state ()
    (is (eql :png (image-format img)))
    (uiop:with-temporary-file (:pathname webp :type "webp")
      (with-wand (wand :file "rose:")
        (save-wand-to-file wand webp))
      (let* ((image (make-image :pathname webp)))
        (is (eql :webp (image-format image)))))))

(test make-image-with-filename
  (with-fixture state ()
   (let ((image (make-image :pathname file)))
     (is (eql +image-state-filesystem+ (%image-state image)))
     (is (path:-e (image-filesystem-pathname image)))
     (is (equalp #(145 184 144 188 213 44 215 112 157 4 202 64 212 94 93 133)
                 (util/digests:md5-file (image-filesystem-pathname image)))))))

(test local-location-for-oid--ensure-directory-is-writeable
  (with-fixture state ()
    (with-open-file (file (local-location-for-oid (mongoid:oid))
                          :direction :output)
      (write-string "hello" file))))

(test update-image
  (with-fixture state ()
    (let ((hash #(145 184 144 188 213 44 215 112 157 4 202 64 212 94 93 133)))
     (let ((image (make-image :hash hash)))
       (update-image image :pathname file)
       (with-local-image (file image)
         (is (equalp hash
                     (md5-file file))))))))

(test overwriting-existing-image-warns
  (with-fixture state ()
    (let ((hash #(145 184 144 188 213 44 215 112 157 4 202 64 212 94 93 133)))
      (let ((image (make-image :hash hash)))
        (handler-case
            (update-image image :pathname file)
          (image-reuploaded-warning ()
            (fail "Did not expect to see image-reuploaded-warning")))

        (signals image-reuploaded-warning
          (update-image image :pathname file))
        
        (with-local-image (file image)
          (is (equalp hash
                      (md5-file file))))))))

(test find-image-by-oid
  (with-fixture state ()
    (let ((img (make-image :pathname file)))
      (is (not (null (oid img))))
      (is (eql img (find-image-by-oid (oid img))))
      (is (eql 12 (length (oid-array img))))
      (is (eql img (find-image-by-oid (oid-array img)))))))

(test negative-width-for-mask-rect
  (with-fixture state ()
   (let ((mask-rect (make-instance 'mask-rect
                                   :top 0
                                   :left 10
                                   :width -5
                                   :height 5)))
     (is (eql 0 (mask-rect-top mask-rect)))
     (is (eql 5 (mask-rect-left mask-rect)))
     (is (eql 5 (mask-rect-width mask-rect)))
     (is (eql 5 (mask-rect-height mask-rect))))))

(test negative-height-for-mask-rect
  (with-fixture state ()
   (let ((mask-rect (make-instance 'mask-rect
                                   :top 0
                                   :left 10
                                   :width 5
                                   :height -5)))
     (is (eql -5 (mask-rect-top mask-rect)))
     (is (eql 10 (mask-rect-left mask-rect)))
     (is (eql 5 (mask-rect-width mask-rect)))
     (is (eql 5 (mask-rect-height mask-rect))))))


(test soft-expiration-time
  ;; The date this oid was generated Jan 18th, 2023
  (let ((today (local-time:parse-timestring "2023-01-18T00:00:00Z"))
        (oid (make-oid :arr (mongoid:oid "63c82a1bf6fe3c0c419f630a"))))
    (let ((expiry (soft-expiration-time oid)))
      (is (local-time:timestamp<
               expiry
               (local-time:timestamp+ today 124 :day)))
      (is (local-time:timestamp>
           expiry
           (local-time:timestamp+ today 58 :day))))))

(test all-soft-expired-images-happy-path
  (with-fixture state ()
    (is-false (all-soft-expired-images))))

(test no-image-uploaded-yet
  (with-fixture state ()
    (setf (%image-state img) nil)
    (signals no-image-uploaded-yet
      (with-local-image (file img)))))


(test simple-image-snapshots
  (tmpdir:with-tmpdir (dir)
   (with-fixture state (:dir dir)
     (make-image :pathname file)
     (let ((other-image (make-image :pathname file)))
       (slot-makunbound other-image 'company))
     (finishes
       (bknr.datastore:snapshot)))
    (with-fixture state (:dir dir)
      (pass))))

(test store-images-size
  (with-fixture state ()
    (let ((image (make-image :pathname file)))
      (is (eql 1085 (%image-size image))))))

(test store-images-size-on-update
  (with-fixture state ()
    (let ((image (make-image :hash "343431")))
      (update-image image :pathname file)
      (is (eql 1085 (%image-size image))))))

(test img-tmp-dir-exists
  (with-fixture state ()
    (path:-d (img-tmp-dir))))

(test company-image-size
  (with-fixture state ()
    (let ((company (make-instance 'company))
          (total-size 0))
      (let ((image1 (make-image :pathname file :company company))
            (image2 (make-image :pathname file :company company)))
        (update-company-image-size)
        (is (eql (* 2 1085) (get-company-image-size company)))))))

(test company-image-size-when-the-slot-is-NIL
  (with-fixture state ()
    (let ((company (make-instance 'company))
          (total-size 0))
      (let ((image1 (make-image :pathname file :company company))
            (image2 (make-image :pathname file :company company)))
        (setf (%image-size image1) nil)
        (update-company-image-size)
        (is (eql 1085 (get-company-image-size company)))))))

(test company-image-size-with-two-companies
  (with-fixture state ()
    (let ((company1 (make-instance 'company))
          (company2 (make-instance 'company)))
      (let ((image1 (make-image :pathname file :company company1))
            (image2 (make-image :pathname file :company company1))
            (image3 (make-image :pathname file :company company2)))
        (update-company-image-size)
        (is (eql (* 2 1085) (get-company-image-size company1)))
        (is (eql 1085 (get-company-image-size company2)))))))

(test company-image-size-with-no-images
  (with-fixture state ()
    (let ((company (make-instance 'company))
          (total-size 0))
      (update-company-image-size)
      (is (eql 0 (get-company-image-size company))))))

(test with-local-image-raises-image-file-deleted
  (with-fixture state ()
    (let ((image (make-image :pathname file)))
      (let ((local-file (image-filesystem-pathname image)))
        (is (path:-e local-file))
        (delete-file local-file)
        (signals image-file-deleted
          (with-local-image (file image)
            (fail "Should not reach this point")))))))

(test delete-image-deletes-associated-file
  (with-fixture state ()
    (let ((image (make-image :pathname file)))
      (let ((image-file (image-filesystem-pathname image)))
        (is (path:-e image-file))
        (delete-image image)
        (is-false (path:-e image-file))))))

(test delete-image-when-file-already-deleted
  (with-fixture state ()
    (let ((image (make-image :pathname file)))
      (let ((image-file (image-filesystem-pathname image)))
        (is (path:-e image-file))
        (delete-file image-file)
        (is-false (path:-e image-file))
        (finishes
          (delete-image image))))))

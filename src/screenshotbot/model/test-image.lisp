;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/test-image
  (:use #:cl
        #:fiveam
        #:./image)
  (:import-from #:./image
                #:image
                #:local-image
                #:image-blob
                #:%perceptual-hash
                #:image-blob-get
                #:image=)
  (:import-from #:util
                #:oid)
  (:import-from #:screenshotbot/model/image
                #:dimension-width
                #:dimension-height
                #:dimension
                #:image-dimensions
                #:map-unequal-pixels-on-file
                #:map-unequal-pixels-arr
                #:%map-unequal-pixels
                #:fake-mask-rect
                #:map-unequal-pixels)
  (:import-from #:screenshotbot/magick
                #:run-magick)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:export))

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (let* ((img (make-instance 'local-image :url "/assets/images/old-example-view-right.png"))
          (img2 (make-instance 'local-image :url "/assets/images/old-example-view-left.png"))
          (img-copy (make-instance 'local-image :url "/assets/images/old-example-view-right.png"))
          (rect (make-instance 'mask-rect :left 8 :top 11
                                          :height 99 :width 103)))
     (&body))))

(test simple-compare ()
  (with-fixture state ()
    (is-true (image= img img nil))
    (is-true (image= img img-copy nil))
    (is-false (image= img img2 nil))
    (is-true (image= img img2 (list rect)))))


(test image-public-url
  (is (equal "/image/blob/bar/default.png" (util:make-url 'image-blob-get :oid "bar"))))

(test map-unequal-pixels
  (flet ((unequal-pixels (im1 im2 &key masks)
           (let ((ret nil))
             (map-unequal-pixels-arr im1 im2
                                     (lambda (i j)
                                       (push (cons i j) ret))
                                     :masks masks)
             ret)))
   (let ((arr1
           (make-array '(2 2 3) :element-type 'fixnum
                                :initial-contents `(((1 2 3) (1 2 3))
                                                    ((1 2 3) (1 4 6)))))
         (arr2
           (make-array '(2 2 3) :element-type 'fixnum
                                :initial-contents `(((1 2 3) (1 2 3))
                                                    ((1 2 3) (1 5 3))))))
     (is (equal `((1 . 1))
                (unequal-pixels arr1 arr2)))
     (is (equal nil
                (unequal-pixels arr1 arr2
                                :masks (list (make-instance 'fake-mask-rect
                                                            :top 1
                                                            :left 1
                                                            :height 1
                                                            :width 1)))))
     (is (equal `((1 . 1))
                (unequal-pixels arr1 arr2
                                :masks (list (make-instance 'fake-mask-rect
                                                            :top 0
                                                            :left 0
                                                            :height 1
                                                            :width 1))))))))

(test map-unequal-pixels-on-file
  (uiop:with-temporary-file (:pathname f1 :type ".png")
    (uiop:with-temporary-file (:pathname f2 :type ".png")
      (flet ((make-image (x y color output)
               (run-magick
                (list "convert" "-size" "3x3" "xc:skyblue" "-fill" color
                      "-draw" (format nil "point ~a,~a" x y) output))))
        (make-image 2 1 "black" f1)
        (make-image 2 1 "white" f2)
        (let ((ret))
          (map-unequal-pixels-on-file f1 f2
                                      (lambda (x y)
                                        (push (cons x y) ret)))
          ;; y,x
          (is (equal '((1 . 2)) ret))))

      (pass))))

(test image-dimensions
  (with-fixture state ()
    (let* ((file (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
           (blob (make-instance 'image-blob))
           (image (make-instance 'image :blob blob)))
      (uiop:copy-file file (blob-pathname blob))
      (let ((dimension (image-dimensions image)))
        (is (typep dimension 'dimension))
        (is (eql 360 (dimension-height dimension)))
        (is (eql 360 (dimension-width  dimension)))))))

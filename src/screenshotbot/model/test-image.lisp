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
  (:export))

(util/fiveam:def-suite)

(def-fixture state ()
  (let* ((img (make-instance 'local-image :url "/assets/images/old-example-view-right.png"))
         (img2 (make-instance 'local-image :url "/assets/images/old-example-view-left.png"))
         (img-copy (make-instance 'local-image :url "/assets/images/old-example-view-right.png"))
         (rect (make-instance 'mask-rect :left 8 :top 11
                               :height 99 :width 103)))
    (&body)))

(test simple-compare ()
  (with-fixture state ()
    (is-true (image= img img nil))
    (is-true (image= img img-copy nil))
    (is-false (image= img img2 nil))
    (is-true (image= img img2 (list rect)))))


(test perceptual-hash ()
  (with-fixture state ()
    (is (equal "9961a3d5f05ce9847fd54c9a350b0b0c257d156e8c430cc3422226392c888240" (%perceptual-hash img (list rect))))))

(test image-public-url
  (is (equal "/image/blob/bar/default.png" (util:make-url 'image-blob-get :oid "bar"))))

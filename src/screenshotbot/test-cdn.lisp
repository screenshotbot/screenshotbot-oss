;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-cdn
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/cdn
                #:cdn-for-image-url))
(in-package :screenshotbot/test-cdn)

(util/fiveam:def-suite)

(test cdn-for-actual-image-blob
  (let ((util.cdn:*cdn-cache-key* "car"))
    (is (equal "/assets/image/car.png?cache-key=car"
               (cdn-for-image-url "/assets/image/car.png")))
    (is (equal "/image/blob/car.png?cache-key=i6"
               (cdn-for-image-url "/image/blob/car.png")))))

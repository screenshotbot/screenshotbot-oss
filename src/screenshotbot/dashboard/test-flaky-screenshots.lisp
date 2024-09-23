;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-flaky-screenshots
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/dashboard/flaky-screenshots
                #:screenshot-variant-map)
  (:import-from #:screenshotbot/model/company
                #:find-or-create-channel
                #:company)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/model/recorder-run
                #:runs-for-channel
                #:make-recorder-run))
(in-package :screenshotbot/dashboard/test-flaky-screenshots)


(util/fiveam:def-suite)

(defun %make-image (name)
  (make-image
   :pathname (asdf:system-relative-pathname :screenshotbot
                                            (format nil "fixture/~a" name))))

(def-fixture state ()
  (with-test-store ()
    (let* ((company (make-instance 'company
                                   :name "foo"))
           (channel (find-or-create-channel company "foobar"))
           (im1 (%make-image "rose.png"))
           (im2 (%make-image "point.png")))
      
     (&body))))

(test screenshot-variant-map-happy-path
  (with-fixture state ()
    (let ((run (make-recorder-run
                :channel channel
                :screenshots (list
                              (make-screenshot :name "foo"
                                               :image im1)))))
     (finishes
       (screenshot-variant-map (list run))))))

(test screenshot-variant-map-for-multiple-names
  (with-fixture state ()
    (let ((run (make-recorder-run
                :channel channel
                :screenshots (list
                              (make-screenshot :name "foo"
                                               :image im1)
                              (make-screenshot :name "bar"
                                               :image im2)))))
     (finishes
       (screenshot-variant-map (list run))))))

(test ordering
  (with-fixture state ()
    (let ((run (make-recorder-run
                :channel channel
                :screenshots (list
                              (make-screenshot :name "foo"
                                               :image im1)
                              (make-screenshot :name "bar"
                                               :image im2))))
          (run2 (make-recorder-run
                 :channel channel
                 :screenshots (list
                               (make-screenshot :name "foo"
                                                :image im2)
                               (make-screenshot :name "bar"
                                                :image im2)))))
      (let ((result (screenshot-variant-map (list run))))
        (is (equal "foo" (car (first result))))))))

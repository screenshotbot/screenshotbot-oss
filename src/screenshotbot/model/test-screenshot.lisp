;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/test-screenshot
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:../model/channel
        #:../model/screenshot
        #:../model/image
        #:../model/recorder-run
        #:../model/company
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store))

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (let* ((channel (make-instance 'channel)))
     (&body))))

(test no-history
  (with-fixture state ()
    (setf (channel-runs channel) nil)
    (is (equal nil (get-screenshot-history channel "foo")))))

(test theres-runs-but-none-of-this-name
  (with-fixture state ()
    (let* ((run1 (make-instance 'recorder-run
                                :screenshots (list
                                              (make-instance 'screenshot
                                                             :name "foo"
                                                             :image 'im1))
                                :channel channel))
           (run2 (make-instance 'recorder-run
                                :screenshots (list
                                              (make-instance 'screenshot
                                                             :name "foo"
                                                             :image 'im2))
                                :previous-run run1
                                :chanel channel)))
      (setf (active-run channel "master") run2)
      (is-true (recorder-run-screenshots run1))
      (is-true channel)
      (setf (channel-runs channel)
            (list run2 run1))
      (is (equal nil (get-screenshot-history channel "blah")))
      (is (equal (list run2 run1) (channel-promoted-runs channel)))
      (is (equal (list
                  (car (recorder-run-screenshots run2))
                  (car (recorder-run-screenshots run1)))
                 (get-screenshot-history channel "foo"))))))

(test make-screenshot-uniqueness ()
  (with-test-store ()
   (let ((args (list :name "foo4" :lang "bar")))
     (let ((screenshot (apply 'make-screenshot args)))
       (unwind-protect
            (is (eql screenshot (apply 'make-screenshot args)))
         (delete-object screenshot))))))

(test make-screenshot-uniqueness-with-masks ()
  (with-test-store ()
   (let* ((mask1 (list (make-instance 'mask-rect :left 1 :top 2 :width 3 :height 4)))
          (mask2 (list (make-instance 'mask-rect :left 1 :top 2 :width 3 :height 5)))
          (mask3 (list (make-instance 'mask-rect :left 1 :top 2 :width 3 :height 4))))
     (is (eql (make-screenshot :name "foo" :masks mask1)
              (make-screenshot :name "foo" :masks mask3)))
     (is (not (eql (make-screenshot :name "foo" :masks mask1)
                   (make-screenshot :name "foo" :masks mask2)))))))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/test-screenshot
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:screenshotbot/model/channel
        #:screenshotbot/model/screenshot
        #:screenshotbot/model/image
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/company
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/model/image
                #:image=)
  (:import-from #:util/object-id
                #:oid)
  (:import-from #:screenshotbot/model/screenshot
                #:lite-screenshot)
  (:import-from #:screenshotbot/model/screenshot-key
                #:ensure-screenshot-key)
  (:import-from #:bknr.datastore
                #:decode
                #:encode))

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*installation* (make-instance 'installation)))
   (with-test-store ()
     (let* ((channel (make-instance 'channel)))
       (&body)))))

(test no-history
  (with-fixture state ()
    (setf (channel-runs channel) nil)
    (is (equal nil (get-screenshot-history channel "foo")))))

(def-fixture history-fixture ()
  (let* ((im1 (make-image :pathname (asdf:system-relative-pathname :screenshotbot "fixture/rose.png")))
         (im2 (make-image :pathname (asdf:system-relative-pathname :screenshotbot "fixture/wizard.png"))))
    (&body)))

(test theres-runs-but-none-of-this-name
  (with-fixture state ()
    (with-fixture history-fixture ()
      (let* ((run1 (make-instance 'recorder-run
                                   :screenshots (list
                                                 (make-instance 'screenshot
                                                                 :name "foo"
                                                                 :image im1))
                                   :channel channel))
             (run2 (make-instance 'recorder-run
                                   :screenshots (list
                                                 (make-instance 'screenshot
                                                                 :name "foo"
                                                                 :image im2))
                                   :previous-run run1
                                   :channel channel)))
        (is-true (recorder-run-screenshots run1))
        (setf (active-run channel "master") run2)
        (is-true channel)
        (setf (channel-runs channel)
              (list run2 run1))
        (is (equal nil (get-screenshot-history channel "blah")))
        (is (equal (list run2 run1) (channel-promoted-runs channel)))
        (is (equal (list
                    (car (recorder-run-screenshots run2))
                    (car (recorder-run-screenshots run1)))
                   (get-screenshot-history channel "foo")))))))

(test in-history-we-also-pull-renamed-screenshots
  (with-fixture state ()
    (with-fixture history-fixture ()
      (let* ((run1 (make-instance 'recorder-run
                                   :screenshots (list
                                                 (make-instance 'screenshot
                                                                 :name "bar"
                                                                 :image im1))
                                   :channel channel))
             (run2 (make-instance 'recorder-run
                                   :screenshots (list
                                                 (make-instance 'screenshot
                                                                 :name "foo"
                                                                 :image im1))
                                   :previous-run run1
                                   :channel channel)))
        (setf (active-run channel "master") run2)
        (is (equal (list
                    (car (recorder-run-screenshots run2))
                    (car (recorder-run-screenshots run1)))
                   (get-screenshot-history channel "foo")))))))


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


(test image-slot-can-be-oid-or-not
  (with-test-store ()
    (let ((img (make-instance 'image)))
      (let ((s1 (make-instance 'screenshot
                               :name "bleh"
                               :image img)))
        (is (eql img (screenshot-image s1))))
      (let ((s1 (make-instance 'screenshot
                               :name "bleh"
                               :image (oid img :stringp nil))))
        (is (eql img (screenshot-image s1)))))))

(test encode-lite-screenshot
  (with-test-store ()
    (let ((stream (flex:make-in-memory-output-stream))
          (img (make-instance 'image)))
     (let ((screenshot (make-instance 'lite-screenshot
                                      :screenshot-key (ensure-screenshot-key
                                                       :name "foobar")
                                      :image-oid (oid img :stringp nil))))
       (encode screenshot stream)
       (let ((decoded (decode (flex:make-in-memory-input-stream
                               (flex:get-output-stream-sequence  stream))))))))))

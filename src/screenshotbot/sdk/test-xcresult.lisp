;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-xcresult
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/xcresult
                #:parse-name
                #:xcresults-attachment-bundle)
  (:import-from #:screenshotbot/sdk/bundle
                #:image-stream
                #:image-name
                #:list-images)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:fiveam-matchers/lists
                #:has-item
                #:contains))
(in-package :screenshotbot/sdk/test-xcresult)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((bundle (make-instance 'xcresults-attachment-bundle :directory
                               (asdf:system-relative-pathname :screenshotbot.sdk "fixture/xcresults-attachments/"))))
    (&body)))

(test simple-parsing
  (with-fixture state ()
    (assert-that (list-images bundle)
                 (has-length 2))))


(test parse-suggested-name
  (is
   (equal "SimpleProjectTests/testLoginViewSnapshot.1_0"
    (parse-name
     ;; see manifest.json
     "SnapshotTest_testLoginViewSnapshot.1_0_FA32D8D3-B6A9-4B7E-A958-921F53BBC2FE.png"
     "SimpleProjectTests/testLoginViewSnapshot()"))))

(test image-name-works
  (with-fixture state ()
    (assert-that (mapcar #'image-name (list-images bundle))
                 (has-item "SimpleProjectTests/testLoginViewSnapshot.1_0"))))

(defun find-image (bundle name)
  (loop for image in (list-images bundle)
        if (equal name (image-name image))
          return image))

(test image-stream
  (with-fixture state ()
    (with-open-stream (stream (image-stream (find-image bundle "SimpleProjectTests/testLoginViewSnapshot.1_0")))
      (is
       (equal
        "32143da99ce89207d2aa0ecf85843349"
        (str:downcase
         (ironclad:byte-array-to-hex-string (md5:md5sum-stream stream))))))))



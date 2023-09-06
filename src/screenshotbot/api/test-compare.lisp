;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/test-compare
  (:use #:cl
        #:fiveam)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature
                #:installation)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:screenshotbot/api/compare
                #:%api-compare-runs
                #:api-compare-runs)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:screenshotbot/user-api
                #:current-company
                #:current-user)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/test-compare)

(util/fiveam:def-suite)

(defclass my-installation (multi-org-feature
                           installation)
  ())

(def-fixture state ()
  (let ((*installation* (make-instance 'my-installation)))
    (with-test-store ()
      (with-test-user (:user user :company company)
        (with-fake-request ()
         (auth:with-sessions ()
           (tmpdir:with-tmpdir (dir)
             (let ((im1 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
                   (im2 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-2.png"))
                   (im3 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-3.png"))
                   (objs))
               (flet ((make-screenshot (img)
                        (let* ((image (make-image :pathname img :for-tests t)))
                          (make-screenshot
                           :name "foobar"
                           :image image))))
                 (setf (current-user) user)
                 (setf (current-company) company)

                 (&body))))))))))

(test compare-identical-runs
  (with-fixture state ()
    (let ((run (make-recorder-run
                :company company
                :screenshots (list (make-screenshot im1))))
          (run2 (make-recorder-run
                 :company company
                 :screenshots (list (make-screenshot im1)))))
      (let ((compare (%api-compare-runs run run2)))
        (is (eql t (dto:comparison-samep compare)))))))

(test compare-different-runs
  (with-fixture state ()
    (let ((run (make-recorder-run
                :company company
                :screenshots (list (make-screenshot im1))))
          (run2 (make-recorder-run
                 :company company
                 :screenshots (list (make-screenshot im2)))))
      (let ((compare (%api-compare-runs run run2)))
        (is (eql nil (dto:comparison-samep compare)))
        (is (equal "1 changes"
                   (dto:comparison-title compare)))))))

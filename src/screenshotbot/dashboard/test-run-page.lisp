;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-run-page
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:fix-timestamps
                #:with-test-user
                #:screenshot-test
                #:snap-all-images)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:not-fast-forward-promotion-warning
                #:merge-base-failed-warning
                #:recorder-run-warnings
                #:recorder-run)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot)
  (:import-from #:screenshotbot/dashboard/run-page
                #:render-run-page)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:easy-macros
                #:def-easy-macro))
(in-package :screenshotbot/dashboard/test-run-page)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:company company
                     :user user
                     :logged-in-p t)
      (labels ((make-screenshot (img)
                      (let* ((image (make-image :pathname img :for-tests t)))
                        (make-instance 'screenshot
                                       :name "foobar"
                                       :image image))))
        (let* ((channel (make-instance 'channel
                                       :publicp t
                                       :company company
                                       :name "bleh"
                                       :github-repo "git@github.com:a/b.gitq"))
               (im1 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
               (run (make-instance 'recorder-run
                                   :company company
                                   :channel channel
                                   :company company
                                   :screenshots (list (make-screenshot im1))))
               (another-run (make-instance 'recorder-run
                                           :commit-hash "foo")))
          (&body))))))

(def-easy-macro wrap-snapshot (&fn fn)
  (snap-all-images)
  (fix-timestamps (fn)))

(screenshot-test simple-run-page-screenshots
  (with-fixture state ()
    (wrap-snapshot ()
     (render-run-page run))))

(screenshot-test run-page-with-warnings
  (with-fixture state ()
    (with-transaction ()
      (setf (recorder-run-warnings run)
            (list (make-instance 'merge-base-failed-warning
                                 :compared-against another-run))))
    (wrap-snapshot ()
     (render-run-page run))))

(screenshot-test run-page-with-not-fast-forard-warnings
  (with-fixture state ()
    (let ((run2 (make-instance 'recorder-run
                               :company company
                               :channel channel
                               :company company
                               :previous-run run
                               :screenshots (list (make-screenshot im1)))))
     (with-transaction ()
       (setf (recorder-run-warnings run2)
             (list (make-instance 'not-fast-forward-promotion-warning))))
      (wrap-snapshot ()
        (render-run-page run2)))))

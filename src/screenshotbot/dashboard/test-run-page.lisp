;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-run-page
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-test-user
                #:screenshot-test
                #:snap-all-images)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot)
  (:import-from #:screenshotbot/dashboard/run-page
                #:render-run-page))
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
                                   :screenshots (list (make-screenshot im1)))))
          (&body))))))

(screenshot-test simple-run-page-screenshots
  (with-fixture state ()
    (snap-all-images)
    (render-run-page run)))

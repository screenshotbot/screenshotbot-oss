;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/tasks/test-common
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:screenshotbot/api/promote
                #:%maybe-send-tasks)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/task-integration-api
                #:get-issue-content)
  (:import-from #:screenshotbot/tasks/common
                #:noop-task-integration)
  (:import-from #:screenshotbot/model/report
                #:report))
(in-package :screenshotbot/tasks/test-common)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-installation ()
   (with-test-store ()
     (let* ((company (make-instance 'company))
            (img1 (make-image :pathname (asdf:system-relative-pathname :screenshotbot "fixture/rose.png")))
            (screenshot1 (make-screenshot :name "foo" :image img1))
            (channel (make-instance 'channel
                                    :name "channel"
                                    :company company))
            (run (make-recorder-run
                  :channel channel
                  :screenshots (list screenshot1)
                  :company company)))
       (&body)))))

(test happy-path-sending-tasks
  (with-fixture state ()
    (finishes
      (%maybe-send-tasks run))))

(test happy-path-sending-tasks-with-previous-run
  (with-fixture state ()
    (let ((run (make-recorder-run
                :channel channel
                :screenshots nil
                :company company
                :previous-run run)))
     (finishes
       (%maybe-send-tasks run)))))



(test get-issue-content-with-previous-run
  (with-fixture state ()
    (let* ((previous-run (make-recorder-run
                          :channel channel
                          :commit-hash "aaaa"
                          :company company))
           (run (make-recorder-run
                 :channel channel
                 :commit-hash "bbbb"
                 :company company))
           (report (make-instance 'report
                                  :run run
                                  :previous-run previous-run
                                  :channel channel)))
      (is (search "between commits aaaa and bbbb"
                  (get-issue-content (make-instance 'noop-task-integration
                                                    :company company)
                                     report))))))

(test get-issue-content-without-previous-run
  (with-fixture state ()
    (let* ((run (make-recorder-run
                 :channel channel
                 :commit-hash "bbbb"
                 :company company))
           (report (make-instance 'report
                                  :run run
                                  :previous-run nil
                                  :channel channel)))
      (is (search "added in commit bbbb"
                  (get-issue-content (make-instance 'noop-task-integration
                                                    :company company)
                                     report))))))

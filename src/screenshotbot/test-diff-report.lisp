;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/test-diff-report
    (:use #:cl #:alexandria #:fiveam)
  (:import-from #:screenshotbot/model
                #:channel
                #:make-screenshot
                #:local-image
                #:recorder-run)
  (:import-from #:screenshotbot/diff-report
                #:group-renamed-p
                #:added-groups
                #:make-image-hashes
                #:*cache*
                #:change
                #:diff-report-deleted
                #:diff-report-added
                #:diff-report
                #:diff-report-changes
                #:diff-report-changes
                #:make-diff-report
                #:diff-report-title)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run))
(in-package :screenshotbot/test-diff-report)

(util/fiveam:def-suite)

(defun static-asset (file)
  (path:catfile
   #.(asdf:system-relative-pathname :screenshotbot
                                    "static/")
   file))

(def-fixture state ()
  (let ((*installation* (make-instance 'installation))
        (*cache* (make-hash-table :test #'equal)))
   (with-test-store ()
     (let* ((img (make-image
                  :pathname (static-asset "assets/images/example-view-square.svg.png")))
            (img2 (make-image
                   :pathname (static-asset "assets/images/example-view.svg.png")))
            (channel (make-instance 'channel))
            (screenshot (make-screenshot :name "foo"
                                         :image img))
            (screenshot2 (make-screenshot :name "foo"
                                          :image img2))
            (run1 (make-recorder-run :screenshots (list screenshot)))
            (run2 (make-recorder-run :screenshots (list screenshot2))))
       (flet ((make-change (name)
                (make-instance 'change
                               :before (make-screenshot :name name)
                               :after (make-screenshot :name name))))
         (&body))))))

(test 2-changed
  (with-fixture state ()
    (let ((changes (list
                    (make-change "1")
                    (make-change "2")
                    (make-change "3")))
          (added (list
                  (make-screenshot :name "foo"))))
     (is (equal "3 changes"
                (diff-report-title (make-instance 'diff-report
                                                   :changes changes))))
      (is (equal "3 changes, 1 deleted"
                 (diff-report-title (make-instance 'diff-report :changes changes
                                                                :deleted added))))
      (is (equal "3 changes, 1 added"
                 (diff-report-title (make-instance 'diff-report :changes changes
                                                              :added added)))))))



(test make-diff-report
  (with-fixture state ()
    (let ((diff-report (make-diff-report run1 run2)))
      (is (eql 1 (length (diff-report-changes diff-report)))))))

(test make-diff-report-to-nil-run
  (with-fixture state ()
    (let ((diff-report (make-diff-report run1 nil)))
      (is (eql 1 (length (diff-report-added diff-report)))))))

(test make-diff-report-added
  (with-fixture state ()
    (let ((diff-report (make-diff-report run1 (make-recorder-run
                                               :screenshots '()))))
      (is (eql 0 (length (diff-report-changes diff-report))))
      (is (eql 1 (length (diff-report-added diff-report)))))))


(test make-diff-report-deleted
  (with-fixture state ()
    (let ((diff-report (make-diff-report (make-recorder-run
                                          :screenshots '())
                                         run2)))
      (is (eql 0 (length (diff-report-changes diff-report))))
      (is (eql 1 (length (diff-report-deleted diff-report)))))))


(test make-image-hashes
  (with-fixture state ()
   (let ((screenshots
           (list
            (make-screenshot :image img :name "foo")
            (make-screenshot :image img2 :name "bar"))))
     (is (eql
          2
          (fset:size (make-image-hashes screenshots)))))))

(test group-renamed-p
  (with-fixture state ()
    (let* ((run1 (make-recorder-run
                  :screenshots (list (make-screenshot :image img :name "foo"))))
           (run2 (make-recorder-run
                  :screenshots (list (make-screenshot :image img :name "renamed"))))
           (diff-report (make-diff-report run1 run2)))

      (is (group-renamed-p (car (added-groups diff-report)))))))

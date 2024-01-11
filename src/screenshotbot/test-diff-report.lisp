;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-diff-report
  (:use :cl)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:equal-to)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:it.bese.fiveam
                #:def-fixture
                #:is
                #:test
                #:with-fixture)
  (:import-from #:screenshotbot/diff-report
                #:*cache*
                #:added-groups
                #:change
                #:changes-groups
                #:deleted-groups
                #:diff-report
                #:diff-report-added
                #:diff-report-changes
                #:diff-report-deleted
                #:diff-report-title
                #:group-renamed-p
                #:group-title
                #:make-diff-report
                #:make-image-hashes)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/model/recorder-run
                #:group-separator
                #:make-recorder-run)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:util/store/store
                #:with-test-store))
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


(test grouping-changed
  (with-fixture state ()
    (let* ((run1 (make-recorder-run
                  :screenshots (list (make-screenshot :image img :name "foo--one")
                                     (make-screenshot :image img :name "foo--two"))))
           (run2 (make-recorder-run
                  :screenshots (list (make-screenshot :image img2 :name "foo--one")
                                     (make-screenshot :image img2 :name "foo--two"))))
           (diff-report (make-diff-report run1 run2)))
      (is (eql 1 (length (changes-groups diff-report)))))))

(test grouping-not-grouped-with-different-separator
  (with-fixture state ()
    (let* ((run1 (make-recorder-run
                  :group-separator "!!"
                  :screenshots (list (make-screenshot :image img :name "foo--one")
                                     (make-screenshot :image img :name "foo--two"))))
           (run2 (make-recorder-run
                  :group-separator "!!"
                  :screenshots (list (make-screenshot :image img2 :name "foo--one")
                                     (make-screenshot :image img2 :name "foo--two"))))
           (diff-report (make-diff-report run1 run2)))
      (is (equal "!!" (group-separator run2)))
      (is (equal "!!" (group-separator diff-report)))
      (assert-that (changes-groups diff-report)
                   (has-length 2)))))

(test grouping-changed-with-different-separator
  (with-fixture state ()
    (let* ((run1 (make-recorder-run
                  :group-separator "!!"
                  :screenshots (list (make-screenshot :image img :name "foo!!one")
                                     (make-screenshot :image img :name "foo!!two"))))
           (run2 (make-recorder-run
                  :screenshots (list (make-screenshot :image img2 :name "foo!!one")
                                     (make-screenshot :image img2 :name "foo!!two"))))
           (diff-report (make-diff-report run1 run2)))
      (assert-that (changes-groups diff-report)
                   (has-length 1)))))


(test grouping-added-removed
  (with-fixture state ()
    (let* ((run1 (make-recorder-run
                  :screenshots (list (make-screenshot :image img :name "foo--one")
                                     (make-screenshot :image img :name "foo--two"))))
           (run2 (make-recorder-run
                  :screenshots (list (make-screenshot :image img2 :name "bar--one")
                                     (make-screenshot :image img2 :name "bar--two"))))
           (diff-report (make-diff-report run1 run2)))
      (assert-that (added-groups diff-report)
                   (has-length 1))
      (assert-that (deleted-groups diff-report)
                   (has-length 1)))))

(test grouping-added-removed-not-grouped-with-different-separator
  (with-fixture state ()
    (let* ((run1 (make-recorder-run
                  :group-separator "!!"
                  :screenshots (list (make-screenshot :image img :name "foo--one")
                                     (make-screenshot :image img :name "foo--two"))))
           (run2 (make-recorder-run
                  :group-separator "!!"
                  :screenshots (list (make-screenshot :image img2 :name "bar--one")
                                     (make-screenshot :image img2 :name "bar--two"))))
           (diff-report (make-diff-report run1 run2)))
      (assert-that (added-groups diff-report)
                   (has-length 2))
      (assert-that (deleted-groups diff-report)
                   (has-length 2)))))

(test grouping-added-removed-with-diff-separator
  (with-fixture state ()
    (let* ((run1 (make-recorder-run
                  :group-separator "!!"
                  :screenshots (list (make-screenshot :image img :name "foo!!one")
                                     (make-screenshot :image img :name "foo!!two"))))
           (run2 (make-recorder-run
                  :screenshots (list (make-screenshot :image img2 :name "bar!!one")
                                     (make-screenshot :image img2 :name "bar!!two"))))
           (diff-report (make-diff-report run1 run2)))
      (assert-that (added-groups diff-report)
                   (has-length 1))
      (assert-that (group-title (car (added-groups diff-report)))
                   (equal-to "foo"))
      (assert-that (deleted-groups diff-report)
                   (has-length 1))
      (assert-that (group-title (car (deleted-groups diff-report)))
                   (equal-to "bar")))))

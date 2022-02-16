;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/diff-report
  (:use #:cl)
  (:import-from #:screenshotbot/user-api
                #:Screenshot-name
                #:recorder-run-screenshots)
  (:import-from #:screenshotbot/model/image
                #:image=)
  (:import-from #:screenshotbot/screenshot-api
                #:screenshot-image)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot-masks)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:change
   #:before
   #:after
   #:change-masks
   #:diff-report-added
   #:diff-report-deleted
   #:diff-report-changes
   #:make-diff-report))
(in-package :screenshotbot/diff-report)

(defclass change ()
  ((before :initarg :before
           :reader before)
   (after :initarg :after
          :reader after)
   (masks :initarg :masks
          :reader change-masks)))

(defclass diff-report ()
  ((added :initarg :added
          :reader diff-report-added
          :initform nil)
   (deleted :initarg :deleted
            :reader diff-report-deleted
            :initform nil)
   (changes :initarg :changes
            :initform nil
            :accessor diff-report-changes
            :documentation "List of all CHANGEs")))

(defun make-diff-report (run to)
  (restart-case
      (flet ((screenshot-name= (x y)
               (string= (screenshot-name x) (screenshot-name y))))
        (let ((names (recorder-run-screenshots run))
              (to-names (recorder-run-screenshots to)))
          (make-instance
           'diff-report
           :added (set-difference names to-names :test #'screenshot-name=)
           :deleted (set-difference to-names names :test #'screenshot-name=)
           :changes (loop for s1 in names appending
                                          (loop for x in to-names
                                                if (and
                                                    (string= (screenshot-name s1) (Screenshot-name x))
                                                    (not (image= (screenshot-image s1)
                                                                 (Screenshot-image x)
                                                                 ;; always use the new mask
                                                                 (screenshot-masks s1))))
                                                  collect
                                                  (make-instance 'change
                                                                 :before s1
                                                                 :masks (screenshot-masks s1)
                                                                 :after x))))))
    (retry-make-diff-report ()
      (make-diff-report run to))))

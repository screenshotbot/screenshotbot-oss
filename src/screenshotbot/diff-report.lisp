;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/diff-report
  (:use #:cl)
  (:import-from #:screenshotbot/user-api
                #:Screenshot-name)
  (:import-from #:screenshotbot/model/image
                #:image=)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-screenshots)
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
   #:make-diff-report
   #:diff-report
   #:diff-report-title))
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

(defun diff-report-title (diff-report)
  (let ((added (diff-report-added diff-report))
        (deleted (diff-report-deleted diff-report))
        (changes (diff-report-changes diff-report)))
    (str:join ", "
              (remove-if 'null
               (list
                (when changes
                  (format nil "~d changes" (length changes)))
                (when deleted
                  (format nil "~d deleted" (length deleted)))
                (when added
                  (format nil "~d added" (length added))))))))

(defun hash-set-difference (left right &key test (key #'identity))
  "Similar to set-difference, but more performant"
  (let ((table (make-hash-table :test test)))
    (dolist (x left)
      (setf (gethash (funcall key x) table) x))
    (dolist (x right)
      (remhash (funcall key x) table))
    (alexandria:hash-table-values table)))

(defun make-diff-report (run to)
  (restart-case
      (let ((names (recorder-run-screenshots run))
            (to-names (recorder-run-screenshots to)))
        (make-instance
         'diff-report
          :added (hash-set-difference
                  names to-names
                  :key #'screenshot-name
                  :test #'equal)
          :deleted (hash-set-difference
                    to-names names
                    :key #'screenshot-name
                    :test #'equal)
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
                                                                 :after x)))))
    (retry-make-diff-report ()
      (make-diff-report run to))))

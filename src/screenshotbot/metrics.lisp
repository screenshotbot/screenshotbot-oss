;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/metrics
  (:use #:cl
        #:screenshotbot/model/company
        #:screenshotbot/model/user
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/report))
(in-package :screenshotbot/metrics)

(defvar *company* #+nil(company-with-name ""))

(defun number-of-members ()
  (let ((results nil))
    (loop for interval in (list 30 90) do
      (flet ((metric (name val)
               (setf (alexandria:assoc-value
                      results
                      (format nil "~a-~a" name interval) )
                     val)))
        (metric :num-users
                (length (users-for-company *company*)))
        (let ((runs (loop for run in (fset:convert 'list (runs-for-company *company*))
                          if (local-time:timestamp> (screenshotbot/user-api:created-at run) (local-time:timestamp- (local-time:now) interval :day))
                            collect run)))
          (metric :num-runs (length runs))

          (let ((channels (remove-duplicates
                           (mapcar #'recorder-run-channel runs))))
            (metric :active-channels
                    (length channels))
            (metric :active-screenshots
             (loop for channel in channels
                   summing
                   (length
                    (remove-duplicates
                     (loop for (branch . run) in (screenshotbot/model/channel::all-active-runs channel)
                           appending
                           (mapcar #'screenshotbot/model/screenshot:screenshot-name
                                   (recorder-run-screenshots run))))))))
          (let ((pr-runs (loop for run in runs
                               if (pull-request-url run)
                                 collect run)))
            (metric :num-runs-with-pr
                    (length pr-runs))
            (let ((reports (loop for run in pr-runs
                                 appending (reports-for-run run))))
              (metric :reports (length reports))
              (let ((reviewed-reports
                      (loop for report in reports
                            if (acceptable-state (report-acceptable report))
                              collect report)))
                (metric :reviewed-reports (length reviewed-reports))
                (let ((reviewers
                        (loop for report in reviewed-reports
                              collect (acceptable-reviewer (report-acceptable report)))))
                  (metric :active-reviewers (length (remove-duplicates reviewers)))

                  (metric :top-reviewers (top-5 reviewers)))))))))
    results))

(defun top-5 (items)
  (let ((ht (make-hash-table)))
    (dolist (item items)
      (incf (gethash item ht 0)))
    (let ((items (loop for key being the hash-keys of ht
                       for count being the hash-values of ht
                       collect (list key count))))
      (setf items (sort items #'> :key #'second))
      (cond
        ((< (length items) 5)
         items)
        (t
         (subseq items 0 6))))))

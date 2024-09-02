;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/pull-requests
  (:use #:cl)
  (:import-from #:screenshotbot/model/recorder-run
                #:phabricator-diff-id
                #:gitlab-merge-request-iid)
  (:import-from #:screenshotbot/user-api
                #:pull-request-url)
  (:import-from #:screenshotbot/insights/runs
                #:runs-for-last-60-days)
  (:import-from #:screenshotbot/model/report
                #:acceptable-history-item-user
                #:acceptable-history
                #:acceptable-state
                #:reports-for-run)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:screenshotbot/report-api
                #:report-acceptable))
(in-package :screenshotbot/insights/pull-requests)

(defun safe-pr (run)
  (or
   (pull-request-url run)
   (gitlab-merge-request-iid run)
   ;; This is incorrect, needs revision here:
   (phabricator-diff-id run)))

(defun pr-to-actions (company)
  (let ((actions (make-hash-table :test #'equal))
        (runs (runs-for-last-60-days company :num-days 30)))
    (loop for run in runs
          if (safe-pr run)
          do (setf (gethash (safe-pr run) actions)
                   :none))
    (loop for run in runs do
      (loop for report in (reports-for-run run) do
        (when-let ((acceptable (report-acceptable report)))
          (case (acceptable-state acceptable)
            (:rejected
             (setf (gethash (safe-pr run) actions)
                   :rejected))
            (:accepted
             (when (eql :none (gethash (safe-pr run) actions))
               (setf (gethash (safe-pr run) actions)
                     :accepted)))))))
    actions))

(defun user-reviews-last-30-days (company)
  (let ((runs (runs-for-last-60-days company :num-days 30))
        (result (make-hash-table)))
    (dolist (run runs)
      (dolist (report (reports-for-run run))
        (when-let ((acceptable (report-acceptable report)))
          (dolist (user
                   (remove-duplicates
                    (loop for history-item in (acceptable-history acceptable)
                          collect (acceptable-history-item-user history-item))))
            (incf (gethash user result 0))))))
    (sort
     (loop for user being the hash-keys of result
             using (hash-value count)
           collect (list user count))
     #'>
     :key #'second)))

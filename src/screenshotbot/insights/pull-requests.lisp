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
                #:report-acceptable)
  (:import-from #:screenshotbot/insights/variables
                #:*num-days*))
(in-package :screenshotbot/insights/pull-requests)

(defun safe-pr (run)
  (or
   (pull-request-url run)
   (gitlab-merge-request-iid run)
   ;; This is incorrect, needs revision here:
   (phabricator-diff-id run)))

(defvar *reports-for-run* (make-hash-table :test #'eql))

(defun fast-reports-for-run (run)
  (cond
    ((> (screenshotbot/model/recorder-run::%created-at run)
        (- (get-universal-time)
           3600))
     (reports-for-run run))
    (t
     (cdr
      (util:or-setf
       (gethash run *reports-for-run*)
       (list*
        :dummy
        (reports-for-run run)))))))

(easy-macros:def-easy-macro do-run-report (&binding run &binding report
                                                    company
                                                    &key num-days
                                                    &fn fn)
  (loop for run in (runs-for-last-60-days company :num-days num-days) do
    (loop for report in (fast-reports-for-run run) do
      (fn run report))))

(defun pr-to-actions (company)
  (let ((actions (make-hash-table :test #'equal))
        (runs (runs-for-last-60-days company :num-days 30)))
    (loop for run in runs
          if (safe-pr run)
          do (setf (gethash (safe-pr run) actions)
                   :none))
    (do-run-report (run report company :num-days 30)
      (when (eql :none (gethash (safe-pr run) actions))
        (setf (gethash (safe-pr run) actions)
              :changed))
      (when-let ((acceptable (report-acceptable report)))
        (case (acceptable-state acceptable)
          (:rejected
           (setf (gethash (safe-pr run) actions)
                 :rejected))
          (:accepted
           (when (eql :changed #| should not be :none |#
                      (gethash (safe-pr run) actions))
             (setf (gethash (safe-pr run) actions)
                   :accepted))))))
    actions))

(defun pr-to-actions-to-csv (company output)
  "Meant to sending over this data manually to customers"
  (with-open-file (output output :direction :output)
    (loop for pr being the hash-keys of (pr-to-actions company)
            using (hash-value state)
          do
             (format output "~a,~a~%" pr (string-downcase state)))))

(defun user-reviews-last-30-days (company)
  (let ((result (make-hash-table)))
    (do-run-report (run report company :num-days *num-days*)
      (when-let ((acceptable (report-acceptable report)))
        (dolist (user
                 (remove-duplicates
                  (loop for history-item in (acceptable-history acceptable)
                        collect (acceptable-history-item-user history-item))))
          (incf (gethash user result 0)))))
    (sort
     (loop for user being the hash-keys of result
             using (hash-value count)
           collect (list user count))
     #'>
     :key #'second)))

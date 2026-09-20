;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/pull-requests
  (:use #:cl)
  (:import-from #:screenshotbot/model/recorder-run
                #:%run-build-url
                #:build-url
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
                #:report-run
                #:report-acceptable)
  (:import-from #:screenshotbot/insights/variables
                #:*num-days*)
  (:import-from #:screenshotbot/dashboard/review-link
                #:get-canonical-pull-request-url)
  (:import-from #:screenshotbot/model/recorder-run
                #:pull-request-id)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-channel
                #:channel-repo)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/dashboard/reports
                #:report-link))
(in-package :screenshotbot/insights/pull-requests)

(defun canonical-pr-url (run)
  "The pull request URL on the run is whatever the CI reported, so it
can look like git@github.com:foo/bar.git/pull/42. Rebuild it the same
way the dashboard builds review links. Returns NIL if we don't know
how to canonicalize it for this run's forge."
  (when-let ((id (pull-request-id run)))
    (let ((url (get-canonical-pull-request-url
                (?. channel-repo (recorder-run-channel run))
                id)))
      ;; The default method returns "#", which would otherwise collapse
      ;; every unrecognized repo into a single key.
      (unless (equal "#" url)
        url))))

(defun safe-pr (run)
  (or
   (canonical-pr-url run)
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

(defun pr-to-actions (company &key (num-days *num-days*))
  (let ((actions (make-hash-table :test #'equal))
        (runs (runs-for-last-60-days company :num-days num-days))
        (failure-examples (make-hash-table :test #'equal)))
    (loop for run in runs
          if (safe-pr run)
          do (setf (gethash (safe-pr run) actions)
                   :none))
    (do-run-report (run report company :num-days num-days)
      (when (eql :none (gethash (safe-pr run) actions))
        (setf (gethash (safe-pr run) actions)
              :changed)
        (setf (gethash (safe-pr run) failure-examples) report))
      (when-let ((acceptable (report-acceptable report)))
        (case (acceptable-state acceptable)
          (:rejected
           (setf (gethash (safe-pr run) actions)
                 :rejected)
           (setf (gethash (safe-pr run) failure-examples) report))
          (:accepted
           (when (eql :changed #| should not be :none |#
                      (gethash (safe-pr run) actions))
             (setf (gethash (safe-pr run) actions)
                   :accepted))))))
    (values actions failure-examples)))

(defun csv-cell (value)
  "Render VALUE as a single CSV cell, quoting it if required."
  (let ((value (if value (format nil "~a" value) "")))
    (if (find-if (lambda (ch)
                   (member ch '(#\, #\" #\Newline #\Return)))
                 value)
        (format nil "\"~a\"" (str:replace-all "\"" "\"\"" value))
        value)))

(defun write-pr-actions-csv (company output &key (num-days *num-days*))
  "Write the per-PR data behind the Insights pull requests chart as CSV."
  (multiple-value-bind (actions failure-examples)
      (pr-to-actions company :num-days num-days)
    (format output
            "PR URL,STATUS,REPORT URL,BUILD URL~%")
    (loop for pr being the hash-keys of actions
            using (hash-value state)
          do
             (format output "~{~a~^,~}~%"
                     (mapcar #'csv-cell
                             (list pr (string-downcase state)
                                   (util/misc:?.
                                    report-link
                                    (gethash pr failure-examples))
                                   (util/misc:?.
                                    %run-build-url
                                    (util/misc:?. report-run (gethash pr failure-examples)))))))))

(defun pr-to-actions-to-csv (company output &key (num-days 60))
  "Meant to sending over this data manually to customers"
  (with-open-file (output output :direction :output :if-exists :supersede)
    (write-pr-actions-csv company output :num-days num-days)))

(defun user-reviews-last-n-days (company &key (num-days *num-days*))
  (let ((result (make-hash-table)))
    (do-run-report (run report company :num-days num-days)
      (when-let ((acceptable (report-acceptable report)))
        (dolist (user
                 (remove-duplicates
                  (loop for history-item in (acceptable-history acceptable)
                        collect (acceptable-history-item-user history-item))))
          (incf (gethash user result 0)))))
    (sort
     (loop for user being the hash-keys of result
             using (hash-value count)
           if user ;; temp fix for T1872
           collect (list user count))
     #'>
     :key #'second)))

(defun user-reviews-last-n-days-csv (company &key num-days output)
  "Prints the user-reviews for num-days as a CSV into stdout"
  (with-open-file (out output :direction :output)
    (format out "EMAIL,NUMBER OF REVIEWS~%")
    (loop for (user count) in (user-reviews-last-n-days company :num-days num-days)
          do
             (format out "~a,~a~%" (auth:user-email user) count))))


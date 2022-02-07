;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/tasks/common
  (:use #:cl
        #:alexandria
        #:screenshotbot/task-integration-api
        #:screenshotbot/promote-api
        #:screenshotbot/model/company
        #:screenshotbot/api/promote
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/report
        #:screenshotbot/model/screenshot
        #:screenshotbot/dashboard/reports
        #:screenshotbot/compare)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/compare
                #:warmup-comparison-images))
(in-package :screenshotbot/tasks/common)

(defclass noop-task-integration (task-integration)
  ())

(register-task-integration 'noop-task-integration)


(defmethod get-enabled-task-integrations (company channel)
  (remove-if-not
   'enabledp
    (loop for name in (task-integrations-list)
          collect
          (make-instance name :company company))))


(defmethod get-issue-content ((inst task-integration) report)
  (declare (ignore inst))
  (let ((previous-run (report-previous-run report))
        (run (report-run report)))
   (format nil
           "Some screenshots have changed between commits ~a and ~a

See ~a to view the changes.

Don't panic! This is might not be a regression! Usually screenshot changes are intentional, but it's still good to review the changes to make sure it's not a regression."
           (recorder-run-commit previous-run)
           (recorder-run-commit run)
           (report-link report))))


(defmethod send-task ((inst noop-task-integration) report)
  nil)

(defmethod enabledp ((inst noop-task-integration))
  t)

(defmethod maybe-send-tasks ((promoter master-promoter) run)
  (%maybe-send-tasks run))

(defun %maybe-send-tasks (run)
  "Send any tasks if required, called from a background job without
  any db."
  (restart-case
      (let* ((previous-run (recorder-previous-run run))
             (channel (recorder-run-channel run))
             (company (recorder-run-company run)))
        (when previous-run
          (let ((diff-report (make-diff-report run previous-run)))
            (cond
              ((not previous-run) ;; todo: assert `WAS-ACTIVE-P`?
               (log:info "Doesn't look like master run"))
              ((diff-report-empty-p diff-report)
               (log:info "Diff report is empty, not sending any tasks"))
              (t
               (log:info "Screenshotbot is ready to send out a task, fingers crossed")
               (let* ((report (let ((report (make-instance 'report
                                                           :run run
                                                           :channel channel
                                                           :num-changes (length (diff-report-changes diff-report))
                                                           :previous-run previous-run
                                                           :title (diff-report-title diff-report))))
                                (with-transaction ()
                                 (push report
                                       (company-reports (recorder-run-company run))))
                                report)))
                 ;; This is only used by the frontend when viewed
                 ;; by the user, it's not used in the promotion
                 ;; flows.
                 (warmup-comparison-images run previous-run)
                 (dolist (task-integration (get-enabled-task-integrations company channel))
                   (let ((task-integration task-integration))
                     (labels ((thread ()
                                (restart-case
                                    (send-task task-integration report)
                                  (retry-task-integration ()
                                    (thread))
                                  (ignore-task-integation ()
                                    nil))))
                       (bt:make-thread #'thread))))))))))
      (dangerous-retry-full-send-task-flow ()
        (%maybe-send-tasks run))))

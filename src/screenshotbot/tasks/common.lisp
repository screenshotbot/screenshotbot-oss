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
                #:warmup-comparison-images)
  (:import-from #:screenshotbot/diff-report
                #:diff-report-changes
                #:make-diff-report
                #:diff-report-empty-p
                #:diff-report-title)
  (:import-from #:screenshotbot/model/company
                #:add-company-report)
  (:import-from #:screenshotbot/webhook/model
                #:webhook-payload)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:screenshotbot/user-api
                #:channel-name)
  (:import-from #:screenshotbot/api/recorder-run
                #:run-to-dto)
  (:import-from #:screenshotbot/webhook/webhook
                #:send-webhook)
  (:import-from #:screenshotbot/model/report
                #:report-to-dto)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
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

(defclass channel-promoted-payload (webhook-payload)
  ((channel :initarg :channel
            :json-key "channel"
            :json-type :string
            :documentation "The channel name on which this promotion happened.")
   (branch :initarg :branch
           :json-key "branch"
           :json-type (or null :string)
           :documentation "The git branch on which this promotion happened. e.g. `main`")
   (run :initarg :run
        :json-key "run"
        :json-type dto:run
        :documentation "The run that triggered this event")
   (previous-run :initarg :previous-run
                :json-key "previousRun"
                 :json-type dto:run
                 :documentation "The previous promoted run")
   (report :initarg :report
           :json-key "report"
           :json-type dto:report
           :documentation "The report object that was generated"))
  (:documentation "This event is dispatched when a promotion happens on a branch that
    resulted in a report.")
  (:metaclass ext-json-serializable-class)
  (:default-initargs :event "channel.promotion"))

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
                                                           :promotion-report-p t
                                                           :channel channel
                                                           :num-changes (length (diff-report-changes diff-report))
                                                           :previous-run previous-run
                                                           :title (diff-report-title diff-report))))
                                (add-company-report
                                 (recorder-run-company run)
                                 report)
                                report)))
                 ;; This is only used by the frontend when viewed
                 ;; by the user, it's not used in the promotion
                 ;; flows.
                 (warmup-comparison-images run previous-run)
                 (send-webhook company
                               (make-instance 'channel-promoted-payload
                                              :channel (channel-name channel)
                                              :branch (recorder-run-branch run)
                                              :run (run-to-dto run)
                                              :previous-run (run-to-dto previous-run)
                                              :report (report-to-dto report)))
                 (dolist (task-integration (get-enabled-task-integrations company channel))
                   (let ((task-integration task-integration))
                     (labels ((thread ()
                                (restart-case
                                    (send-task task-integration report)
                                  (retry-task-integration ()
                                    (thread))
                                  (ignore-task-integation ()
                                    nil))))
                       (util:make-thread #'thread))))))))))
      (dangerous-retry-full-send-task-flow ()
        (%maybe-send-tasks run))))

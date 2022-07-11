;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/slack/task-integration
  (:use #:cl
        #:alexandria
        #:screenshotbot/task-integration-api
        #:screenshotbot/model/channel
        #:screenshotbot/model/report
        #:screenshotbot/model/company
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/report
        #:anaphora
        #:screenshotbot/slack/core)
  (:import-from #:screenshotbot/dashboard/reports
                #:report-link)
  (:import-from #:screenshotbot/slack/core
                #:slack-error))
(in-package :screenshotbot/slack/task-integration)

(defclass slack-task-integration (task-integration)
  ())

(register-task-integration 'slack-task-integration)

(defmethod enabledp ((inst slack-task-integration))
  (let ((company (task-integration-company inst)))
    (awhen (default-slack-config company)
      (enabledp it))))

(defmethod send-task ((inst slack-task-integration) report)
  (let ((company (task-integration-company inst)))
    (assert (enabledp inst))
    (let ((it (default-slack-config company)))
      (handler-case
          (slack-post-on-channel
           :channel (slack-config-channel it)
           :company company
           :token (access-token (access-token it))
           :text (format nil "Screenshots changed in ~a, see: ~a"
                         (channel-name (report-channel report))
                         (report-link report)))
        (slack-error (e)
          ;; the slack API error has already been logged, so we should
          ;; not propagate this.
          (values))))))

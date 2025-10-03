;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/microsoft-teams/task-integration
  (:use #:cl
        #:alexandria
        #:screenshotbot/task-integration-api
        #:screenshotbot/model/channel
        #:screenshotbot/model/report
        #:screenshotbot/model/company
        #:screenshotbot/model/recorder-run
        #:anaphora)
  (:import-from #:screenshotbot/dashboard/reports
                #:report-link)
  (:import-from #:screenshotbot/microsoft-teams/teams-api
                #:make-adaptive-card
                #:teams-post-adaptive-card
                #:teams-error)
  (:import-from #:screenshotbot/microsoft-teams/model
                #:teams-workflows-for-channel
                #:webhook-url
                #:teams-workflow)
  (:import-from #:screenshotbot/model/channel
                #:channel-teams-workflows)
  (:import-from #:screenshotbot/model/company
                #:default-teams-config)
  (:import-from #:screenshotbot/model/report
                #:report-channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-tags
                #:recorder-run-work-branch)
  (:import-from #:util/misc
                #:?.))
(in-package :screenshotbot/microsoft-teams/task-integration)

(defclass teams-task-integration (task-integration)
  ())

(register-task-integration 'teams-task-integration)

(defmethod enabledp ((inst teams-task-integration))
  ;; Always enabled since there's no global configuration required
  t)

(defun render-tags (report)
  (let ((tags (recorder-run-tags (report-run report))))
   (cond
     (tags
      (format nil " (from run with tags ~a)" (str:join ", " tags)))
     (t
      ""))))

(defun render-text (report)
  "Render the notification text for a Teams message, similar to Slack format."
  (let ((run (report-run report))
        (first-line (format nil "Screenshots changed in **~a**~a"
                           (channel-name (report-channel report))
                           (render-tags report)))
        (second-line (format nil "[~a](~a)"
                             (report-title report)
                             (report-link report))))
    (format nil "~a~%~a"
            (cond
              ((?. recorder-run-work-branch run)
               (format nil "~a on **~a**" first-line (recorder-run-work-branch run)))
              (t
               first-line))
            second-line)))

(defmethod send-task ((inst teams-task-integration) report)
  (let ((company (task-integration-company inst)))
    (assert (enabledp inst))
    (let ((card-payload (make-adaptive-card :text (render-text report))))
      (let ((workflows (teams-workflows-for-channel (report-channel report))))
        (cerror "here" "here")
        (fset:do-set (workflow workflows)
          (teams-post-adaptive-card
           :webhook-url (webhook-url workflow)
           :card-payload card-payload))))))

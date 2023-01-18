;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/email-tasks/task-integration
  (:use #:cl)
  (:import-from #:screenshotbot/task-integration-api
                #:send-task
                #:enabledp
                #:task-integration-company
                #:register-task-integration
                #:task-integration)
  (:import-from #:screenshotbot/model/user
                #:user-with-email
                #:users-for-company)
  (:import-from #:auto-restart
                #:with-auto-restart)
  (:import-from #:screenshotbot/mailer
                #:send-mail)
  (:import-from #:screenshotbot/installation
                #:installation-domain
                #:installation
                #:mailer*)
  (:import-from #:screenshotbot/user-api
                #:user-email
                #:channel-name
                #:recorder-run-channel
                #:user-email)
  (:import-from #:screenshotbot/report-api
                #:report-run
                #:report-title)
  (:import-from #:screenshotbot/dashboard/reports
                #:report-link)
  (:import-from #:screenshotbot/email-tasks/settings
                #:email-setting
                #:emails-enabledp)
  (:import-from #:util/object-id
                #:oid)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/email-tasks/task-integration)

(markup:enable-reader)

(defclass email-task-integration (task-integration)
  ()
  (:documentation "A task integration that just sends an email when a
  report is created."))

(register-task-integration 'email-task-integration)

(defmethod enabledp ((self email-task-integration))
  ;; TODO: if no other task integrations are enabled, then enable the
  ;; email task-integration.
  t)

(defmethod send-task ((self email-task-integration) report)
  (let ((company (task-integration-company self)))
    (dolist (user (users-for-company company))
      (when (emails-enabledp (email-setting :user user
                                            :company company))
        (send-email-to-user user company report)))
    #-screenshotbot-oss
    (a:when-let ((user (user-with-email "arnold@tdrhq.com")))
      (send-email-to-user user company report))))

(with-auto-restart ()
  (defun send-email-to-user (user company report)
    (push-event :email-task-notification)
    (send-mail
     (mailer*)
     :to (user-email user)
     :subject (format nil "Screenshots changed in ~a" (channel-name (recorder-run-channel (report-run report))))
     :from
     #-screenshotbot-oss "notifications@screenshotbot.io"
     #+screenshotbot-oss nil ;; default mailer setting
     :display-name "Screenshotbot Notifications"
     :reply-to (progn
                 #-screenshotbot-oss "support@screenshotbot.io"
                 #+screenshotbot-oss nil)
     :html-message
     (email-content company report)
     )))

(defun email-content (company report)
  <html>
    <body>
      <p>
        <a href= (report-link report) >
          ,(report-title report)
        </a>
      </p>

      <p style= "color: #222" >
        To stop receiving email notifications for reports
        <a href= (format nil
                  "~a/settings/email-tasks?company=~a"
                  (installation-domain (installation))
                  (oid company))
           >click here</a>.
        You can respond to this email for support from Screenshotbot staff.
      </p>
    </body>
  </html>)

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
                #:can-view
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
  (:import-from #:screenshotbot/model/channel
                #:channel-subscribers
                #:channel-company)
  (:import-from #:screenshotbot/model/report
                #:report-company
                #:report-channel)
  (:import-from #:screenshotbot/dashboard/channels
                #:single-channel-page)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:alexandria
                #:curry)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/email-tasks/task-integration)

(named-readtables:in-readtable markup:syntax)

(defclass email-task-integration (task-integration)
  ()
  (:documentation "A task integration that just sends an email when a
  report is created."))

(register-task-integration 'email-task-integration)

(defmethod enabledp ((self email-task-integration))
  ;; TODO: if no other task integrations are enabled, then enable the
  ;; email task-integration.
  t)

(defun include-arnold (users)
  "Include Arnold in the list of users"
  (cond
    #-screenshotbot-oss
    (t
     (union
      (a:when-let ((user (user-with-email "arnold@tdrhq.com")))
        (list user))
      users))
    (t
     users)))

(defun users-to-email (channel)
  "Who should we email when this channel changes? Returns a list of users."
  (let ((company (channel-company channel)))
    (funcall
     (if (gk:check :cc-arnold (channel-company channel))
         #'include-arnold
         #'identity)
     (union
      (remove-if-not
       (curry #'can-view channel)
       (channel-subscribers channel))
      (remove-if-not
       (lambda (user)
         (emails-enabledp (email-setting :user user
                                         :company company)))
       (users-for-company company))))))

(defmethod send-task ((self email-task-integration) report)
  (dolist (user (users-to-email (report-channel report)))
    (send-email-to-user user report)))

(defun maybe-redact-token (token)
  (if (util:token-safe-for-email-p token)
      token
      "[filtered]"))

(with-auto-restart ()
  (defun send-email-to-user (user report)
    (push-event :email-task-notification)
    (send-mail
     (mailer*)
     :to (user-email user)
     :subject (format nil "Screenshots changed in ~a" (maybe-redact-token (channel-name (recorder-run-channel (report-run report)))))
     :from
     #-screenshotbot-oss "notifications@screenshotbot.io"
     #+screenshotbot-oss nil ;; default mailer setting
     :display-name "Screenshotbot Notifications"
     :reply-to (progn
                 #-screenshotbot-oss "support@screenshotbot.io"
                 #+screenshotbot-oss nil)
     :html-message
     (email-content report))))

(defun %make-full-url (&rest args)
  (format nil "~a~a"
          (installation-domain (installation))
          (apply #'hex:make-url args)))

(defun email-content (report)
  (let ((company (report-company report))
        (channel (report-channel report)))
    <html>
      <body>
        <p>
          <a href= (report-link report) >
            ,(report-title report)
          </a>
        </p>

        <p style= "color: #222" >
          Screenshotbot sends email notifications on main or release branches. Update your global preferences for
          email notifications by           <a href= (%make-full-url
                    "/settings/email-tasks"
                    :company (oid company)) >clicking here</a>.

          You can enable emails for specific channels by subscribing
          to them. Manage your subscription to <tt>,(channel-name channel)</tt>
          by <a href= (%make-full-url 'single-channel-page :id (store-object-id channel)) >clicking here</a>.
        </p>

        <p style= "color: #222">
          You can respond to this email for support from Screenshotbot staff.
        </p>
      </body>
    </html>))

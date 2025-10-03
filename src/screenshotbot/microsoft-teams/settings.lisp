;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/microsoft-teams/settings
  (:use #:cl)
  (:import-from #:markup
                #:deftag)
  (:import-from #:screenshotbot/settings-api
                #:defsettings
                #:should-show-settings-p
                #:settings-template)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:screenshotbot/dashboard/audit-log
                #:render-audit-logs
                #:describe-audit-log)
  (:import-from #:screenshotbot/microsoft-teams/audit-log
                #:teams-audit-log
                #:post-to-workflow-audit-log
                #:workflow-name))
(in-package :screenshotbot/microsoft-teams/settings)

(markup:enable-reader)

(defun get-settings-teams ()
  <settings-template>
    <div class= "card mt-3">
      <div class= "card-header">
        <h3>Microsoft Teams Integration</h3>
      </div>
      <div class= "card-body">
        <p class= "text-muted">
          Microsoft Teams notifications are configured per channel. Visit any channel's settings page to add Teams workflow webhooks.
        </p>
        <p>
          <a href= "/channels" class= "btn btn-primary">Go to Channels</a>
        </p>
      </div>
    </div>

    <audit-logs />
  </settings-template>)

(deftag audit-logs ()
  (render-audit-logs
   :type 'teams-audit-log
   :company (current-company)
   :subtitle "All webhook calls to Microsoft Teams made by Screenshotbot in the last 30 days will be listed here"))

(defmethod describe-audit-log ((self post-to-workflow-audit-log))
  (let ((err (screenshotbot/audit-log:audit-log-error self)))
    (cond
      (err
       <span>Failed to post to workflow <tt>,(workflow-name self)</tt>: ,(progn err)</span>)
      (t
       <span>Posted to workflow <tt>,(workflow-name self)</tt></span>))))

(defsettings teams
  :name "teams"
  :title "Microsoft Teams"
  :section :tasks
  :handler 'get-settings-teams)

(defmethod should-show-settings-p (installation (name (eql 'teams)))
  (gk:check :microsoft-teams (auth:current-company)))

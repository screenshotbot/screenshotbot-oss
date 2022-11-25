;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/slack/settings
  (:use #:cl
        #:alexandria
        #:nibble
        #:screenshotbot/ui
        #:screenshotbot/user-api
        #:util/form-errors
        #:screenshotbot/installation
        #:screenshotbot/model/company
        #:markup
        #:screenshotbot/slack/core
        #:screenshotbot/settings-api)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/slack/plugin
                #:client-id
                #:slack-plugin)
  (:import-from #:screenshotbot/slack/core
                #:audit-log
                #:audit-log-error
                #:slack-channel
                #:slack-error-response
                #:slack-error)
  (:import-from #:screenshotbot/taskie
                #:timeago)
  (:import-from #:screenshotbot/ui/confirmation-page
                #:confirmation-page)
  (:import-from #:screenshotbot/dashboard/paginated
                #:paginated)
  (:import-from #:screenshotbot/dashboard/audit-log
                #:describe-audit-log
                #:render-audit-logs)
  (:export #:post-settings-slack))
(in-package :screenshotbot/slack/settings)

(markup:enable-reader)

(deftag add-to-slack (&key company)
  (with-plugin (slack-plugin)
   (let* ((slack-config (find-or-create-slack-config company))
          (disconnect (nibble (:method :post)
                        (confirmation-page
                         :yes (nibble ()
                                (with-transaction ()
                                  (setf (access-token slack-config) nil)
                                  (hex:safe-redirect "/settings/slack")))
                         :no "/settings/slack"
                         <div>
                           <p>Are you sure you want to disconnect the Slack connection?</p>

                           <p>Reconnecting Slack will require access to your Slack workspace.</p>
                         </div>))))
     (cond
       ((access-token slack-config)
        <div class= "form-group mb-3">
          <input type= "hidden" name= "slack-token"
                 value= (access-token (access-token slack-config)) class= "form-control" />
          <a href= disconnect class= "btn btn-danger" >Disconnect from Slack</a>
        </div>)
       (t
        <div class= "form-group mb-3">
          <a href= (format nil "https://slack.com/oauth/v2/authorize?client_id=~a&scope=chat:write.public,chat:write&user_scope=" (client-id slack-plugin)) ><img alt="Add to Slack" height="40" width="139" src="https://platform.slack-edge.com/img/add_to_slack.png" srcSet="https://platform.slack-edge.com/img/add_to_slack.png 1x, https://platform.slack-edge.com/img/add_to_slack@2x.png 2x" /></a>
          <input type= "hidden" name= "slack-token"
                 value= "unused" class= "form-control" />
        </div>)))))

(defun slack-settings-test (&key slack-token channel)
  (declare (optimize (debug 3)))
  (handler-case
      (progn
        (slack-post-on-channel :channel channel
                               :company (current-company)
                               :token slack-token
                               :text "Test message to test Slack connection")
        <simple-card-page>
          <p>Please verify that the message was sent to the slack channel!</p>
          <a href= "/settings/slack">Back</a>
        </simple-card-page>)
    (slack-error (e)
      <simple-card-page>
        <p>Failed to post to slack: ,(slack-error-response e)</p>
        <a href= "/settings/slack">Back</a>
      </simple-card-page>)))

(defun find-or-create-slack-config (company)
  (or
   (default-slack-config company)
   (make-instance 'slack-config
                   :channel "#general")))

(defun post-settings-slack (default-channel enabledp)
  (multiple-value-bind (token-str last-token) (latest-slack-token (current-company))
    (declare (ignore token-str))
    (let ((errors nil))
      (check-type last-token slack-token)
      (flet ((check (field check message)
               (unless check
                 (push (cons field message)
                       errors))))
        (check :slack-token last-token
               "Please connect your Slack organization")
        (cond
          (errors
           (with-form-errors (:was-validated t
                              :errors errors)
             (get-settings-slack)))
          (t
           (with-transaction ()
             (setf (default-slack-config
                    (current-company))
                   (make-instance 'slack-config
                                   :access-token last-token
                                   :channel default-channel
                                   :enabledp (not (str:emptyp enabledp)))))
           (hex:safe-redirect "/settings/slack")))))))

(defun get-settings-slack ()
    (let ((slack-config
            (find-or-create-slack-config (current-company)))
          (result (nibble (slack-token default-channel enabledp :method :post)
                    (declare (ignore slack-token))
                    (post-settings-slack default-channel enabledp)))
          (test-nibble (nibble (slack-token default-channel :method :post)
                         (slack-settings-test
                          :slack-token slack-token
                          :channel default-channel)) ))
      <settings-template>
            <form action=result method= "POST">

    <div class= "card mt-3">
      <div class= "card-header">
        <h3>Slack Integration</h3>
      </div>
      <div class= "card-body">

        <add-to-slack company= (current-company) />

      <div class= "form-group mb-3">
        <label class= "form-label" for= "default-channel">Slack Default Channel</label>
        <input type= "text" name= "default-channel" id= "default-channel"
               class= "form-control"
               value= (slack-config-channel slack-config) />
      </div>

      <div class= "form-check">
        <input type= "checkbox" name= "enabledp" id= "enabledp"
               class= "form-check-input"
               value= (enabledp slack-config)
               checked= (if (enabledp slack-config) "checked") />
        <label for= "enabledp" class= "form-check-label" >
          Enable Slack Notifications
        </label>
      </div>

      </div>

      <div class= "card-footer">
        <input type= "submit" class= "btn btn-primary" value= "Save" />
        <input type= "submit" class= "btn btn-danger" value= "Test"
               disabled= (unless (access-token slack-config) "disabled")
               formaction=test-nibble />
      </div>


    </div>

    <audit-logs />
      </form>
  </settings-template>))

(deftag audit-logs ()
  (render-audit-logs
   :type 'audit-log
   :company (current-company)
   :subtitle "All API calls to Slack made by Screenshotbot in the last 30 days will be listed here"))

(defmethod describe-audit-log ((self post-on-channel-audit-log))
  <span>Posted on ,(slack-channel self)</span>)


(defsettings slack
  :name "slack"
  :title "Slack"
  :section :tasks
  :plugin 'slack-plugin
  :handler 'get-settings-slack)

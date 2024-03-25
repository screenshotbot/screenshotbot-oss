;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/webhook/settings
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:settings-template
                #:defsettings)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:util/form-errors
                #:with-error-builder
                #:with-form-errors)
  (:import-from #:screenshotbot/model/company
                #:company-admin-p)
  (:import-from #:screenshotbot/user-api
                #:created-at
                #:current-user
                #:current-company)
  (:import-from #:bknr.datastore
                #:delete-object
                #:deftransaction)
  (:import-from #:screenshotbot/webhook/model
                #:event-payload
                #:event
                #:webhook-event
                #:update-config
                #:ensure-webhook-config
                #:enabledp
                #:signing-key
                #:endpoint
                #:webhook-company-config
                #:webhook-config-for-company)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:core/ui/mdi
                #:mdi)
  (:import-from #:parenscript
                #:ps)
  (:import-from #:ps
                #:@)
  (:import-from #:screenshotbot/dashboard/audit-log
                #:render-audit-logs
                #:render-audit-log)
  (:import-from #:screenshotbot/audit-log
                #:audit-log-error)
  (:import-from #:screenshotbot/webhook/webhook
                #:force-resend-webhook
                #:payload)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:core/ui/post
                #:post-a)
  (:import-from #:util/timeago
                #:timeago))
(in-package :screenshotbot/webhook/settings)

(named-readtables:in-readtable markup:syntax)

(defun post-webhook-settings (endpoint signing-key enabled)
  (with-error-builder (:check check :errors errors
                       :form-builder (get-webhook-settings)
                       :form-args (:endpoint endpoint
                                   :signing-key signing-key
                                   :enabled enabled)
                       :success
                       (update-config
                        :company (current-company)
                        :endpoint endpoint
                        :signing-key signing-key
                        :enabled (string-equal "on" enabled)))
    (check :endpoint
           (ignore-errors (quri:parse-uri endpoint))
           "The URL must be valid")
    (check :signing-key
           (>= (length signing-key) 8)
           "Insecure signing key, must be at least 8 characters long.")
    (check nil (company-admin-p
                (current-company)
                (current-user))
           "You must be an admin to update this setting")))

(defun show-signing-key-on-click ()
  (ps
    (let* ((input (get-element-by-id "signing-key"))
           (verb (get-element-by-id "verb"))
           (type (@ input type)))
      (flet ((set (type verb-val)
               (setf (@ input type) type)
               (setf (@ verb inner-h-t-m-l) verb-val)
               ))
        (cond
          ((= type "text")
           (set "password" "Show"))
          (t
           (set "text" "Hide")))))))

(defun get-webhook-settings ()
  (let ((config (ensure-webhook-config (current-company)))
        (post (nibble (endpoint signing-key enabled)
                (post-webhook-settings endpoint signing-key enabled))))
    <settings-template>
      <form method= "post" action= post >
        <div class= "card mt-3">
          <div class= "card-header">
            <h3>Webhooks</h3>
          </div>
          <div class= "card-body">
            <div class= "alert alert-danger d-none" />
            <div class= "form-group mb-3">
              <label for= "endpoint" class= "form-label" >Webhook Endpoint</label>
              <input type= "text" class= "form-control" placeholder= "https://example.com/screenshotbot/webhook" id= "endpoint" name= "endpoint" value= (?. endpoint config) />
            </div>

            <div class= "form-group mb-3">
              <label class= "form-label" for= "signing-key" >Signing Secret Key</label>
              <div class= "input-group mb-2">
                <input type= "password" name= "signing-key" id= "signing-key" class= "form-control"

                       value= (?. signing-key config) />
                <button type= "button" class="btn btn-danger"
                        onclick= (show-signing-key-on-click) >
                  <mdi name= "visibility" /> <span id= "verb">Show</span> Signing Key
                </button>
              </div>

              <div class= "text-muted">
                The signing key can be any string. It will be used to compute an SHA256 HMAC which
                will be sent along with the payload to verify that this was generated by Screenshotbot.
              </div>
            </div>

            <div class= "form-group mb-3">
              <input type= "checkbox" name= "enabled" class= "form-check-input" id= "enabled"
                     checked= (if (?. enabledp config) "checked" nil) />
              <label for= "enabled" class= "form-check-label">Enable webhooks</label>
            </div>
          </div>

          <div class= "card-footer">
            <input type= "submit" class= "btn btn-primary" value= "Save" />
          </div>
        </div>
      </form>

      ,(render-audit-logs :type 'webhook-event
                          :company (current-company)
                          :title "Webhook Event History")
    </settings-template>))

(defmethod render-audit-log ((self webhook-event))
  (let ((failedp (audit-log-error self))
        (details (nibble ()
                   (setf (hunchentoot:content-type*) "application/json")
                   (event-payload self)))
        (retry (nibble (:once t)
                 (force-resend-webhook self)
                 (hex:safe-redirect "/settings/webhook"))))
    <span class= (if failedp "text-danger" "text-success") >
      <mdi name= (if failedp "close" "done") />
      ,(event self) at ,(timeago :timestamp (created-at self))
      <a href=details target= "_blank" >
        <mdi name= "open_in_new" /> Show details
      </a>

      <post-a href= retry title= "Resend Webhook" >
        <mdi name= "refresh" /> Retry
      </post-a>

    </span>) )

(defsettings webhook
  :name "webhook"
  :title "Webhooks"
  :section :developers
  :handler 'get-webhook-settings)

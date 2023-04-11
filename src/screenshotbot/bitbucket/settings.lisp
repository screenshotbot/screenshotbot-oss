;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/bitbucket/settings
  (:nicknames :screenshotbot/pro/bitbucket/settings)
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:settings-template
                #:defsettings)
  (:import-from #:screenshotbot/pro/bitbucket/plugin
                #:bitbucket-plugin-secret
                #:bitbucket-plugin-key
                #:bitbucket-plugin)
  (:import-from #:nibble
                #:nibble-id
                #:nibble)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:screenshotbot/user-api
                #:%created-at
                #:current-company)
  (:import-from #:markup
                #:deftag)
  (:import-from #:screenshotbot/pro/bitbucket/audit-log
                #:with-audit-log
                #:audit-log
                #:access-token-audit-log-grant-type
                #:access-token-audit-log
                #:parse-error-response
                #:audit-log-error
                #:bitbucket-audit-logs-for-company
                #:build-status-audit-log-commit
                #:build-status-audit-log-full-name
                #:build-status-audit-log)
  (:import-from #:core/ui/paginated
                #:paginated)
  (:import-from #:screenshotbot/taskie
                #:timeago)
  (:import-from #:screenshotbot/pro/bitbucket/core
                #:http-success-response?)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/ui/confirmation-page
                #:confirmation-page)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:refresh-token))
(in-package :screenshotbot/bitbucket/settings)

(markup:enable-reader)

(with-class-validation
 (defclass bitbucket-token (store-object)
   ((refresh-token :initarg :refresh-token
                   :reader refresh-token)
    (company :initarg :company
             :reader bitbucket-setting-company
             :index-type hash-index
             :index-reader %bitbucket-settings-for-company)
    (created-at :initarg :created-at))
   (:metaclass persistent-class)
   (:default-initargs :created-at (get-universal-time))))


;; OLD: DO NOT USE.
(defclass bitbucket-setting (store-object)
  ((refresh-token :initarg :refresh-token
                  :reader refresh-token)
   (company :initarg :company)
   (created-at :initarg :created-at))
  (:metaclass persistent-class))

(defun bitbucket-settings-for-company (company)
  (sort (%bitbucket-settings-for-company company) #'>
        :key #'bknr.datastore:store-object-id))

(defun access-token-for-args (args &key company)
  (with-audit-log (audit-log
                   (make-instance 'access-token-audit-log
                                  :company company
                                  :grant-type (a:assoc-value args "grant_type" :test #'string-equal)))
    (let ((plugin (bitbucket-plugin)))
      (multiple-value-bind (stream result-code)
          (util/request:http-request
           (format nil "https://bitbucket.org/site/oauth2/access_token")
           :parameters args
           :basic-authorization (list (bitbucket-plugin-key plugin)
                                      (bitbucket-plugin-secret plugin))
           :method :post
           :want-stream t)
        (let ((body (uiop:slurp-input-stream 'string stream)))
         (cond
           ((http-success-response? result-code)
            (json:decode-json-from-string
             body))
           (t
            (parse-error-response
             body result-code audit-log))))))))

(defun ! (x)
  (assert x)
  x)

(defun update-from-refresh-token (company response)
  #+nil
  (log:info "Updating refresh token from: ~S" response)
  (let ((refresh-token (a:assoc-value response :refresh--token))
        (last-settings (car (bitbucket-settings-for-company company))))
    (assert (not (str:emptyp refresh-token)))
    (when (or (not last-settings)
              (not (equal refresh-token (refresh-token last-settings))))
     (make-instance 'bitbucket-token
                     :refresh-token (! refresh-token)
                     :company company))))

(auto-restart:with-auto-restart ()
  (defun swap-for-token (code)
    (let ((ret
            (access-token-for-args `(("grant_type" . "authorization_code")
                                     ("code" . ,code))
                                   :company (current-company))))
      (clear-tokens)
      (update-from-refresh-token (current-company) ret)

      (hex:safe-redirect "/settings/bitbucket"))))

(defun clear-tokens ()
  (loop for token in (bitbucket-settings-for-company (current-company))
        do (bknr.datastore:delete-object token)))

(defun get-access-token-from-refresh-token (company refresh-token)
  (let* ((response (access-token-for-args `(("grant_type" . "refresh_token")
                                            ("refresh_token" . ,refresh-token))
                                          :company company)))
    (update-from-refresh-token company response)
    (let ((token
            (a:assoc-value
             response
             :access--token)))
     (cond
       (token token)
       (t (error "could not get token: ~a" response))))))

(defun redirect-to-oauth ()

  (let ((callback (nibble (code)
                    (swap-for-token code))))
   (hunchentoot:redirect
    (format nil "https://bitbucket.org/site/oauth2/authorize?client_id=~a&response_type=code&state=~a"
            (bitbucket-plugin-key (bitbucket-plugin))
            (nibble-id callback)))))

(defun disconnect ()
  (let ((actually-disconnect
          (nibble ()
            (clear-tokens)
            (hex:safe-redirect "/settings/bitbucket"))))
    (confirmation-page
     :yes actually-disconnect
     :no "/settings/bitbucket"
     :danger t
     <p>Are you sure you want to disconnect BitBucket? Reconnecting it will require access to your organization BitBucket account.</p>)))

(defun settings-bitbucket-page ()
  (let ()
    <settings-template>
      <div class= "card mt-3">
        <div class= "card-header">
          <h3>BitBucket Integration</h3>
        </div>

        <div class= "card-body">
          <p>Screenshotbot can update build statuses on your BitBucket Pull Requests</p>
        </div>

        <div class= "card-footer">
          ,(cond
             ((bitbucket-settings-for-company (current-company))
              <form method= "post" action= (nibble () (disconnect))>
                <input type= "submit" class= "btn btn-danger" value= "Disconnect" />
              </form>)
             (t <a href= (nibble () (redirect-to-oauth)) class= "btn btn-success">Install on BitBucket</a>))
        </div>
      </div>

      <render-audit-logs />
    </settings-template>))

(deftag render-audit-logs ()
  <div class= "card mt-3 audit-log-card">
    <div class= "card-header">
      <h5>API Audit Logs</h5>
    </div>

  <div class= "card-body">
    <p class= "text-muted">All API calls to BitBucket made by Screenshotbot in the last 30 days will be listed here.</p>

    <ul>
      ,(paginated
        (lambda (x)
          <li>,(render-audit-log-item x)</li>)
        :items (bitbucket-audit-logs-for-company (current-company)))
    </ul>
  </div>
  </div>)

(defmethod %timeago ((self audit-log))
  (timeago :timestamp (%created-at self)))

(defmethod render-audit-log-item ((self build-status-audit-log))
  (let ((commit (build-status-audit-log-commit self))
        (err (audit-log-error self)))
    <span class= (if err "text-danger" nil) >,(if err "Failed to push" "Pushed") build status for commit <code title= commit >,(str:shorten 8 commit)</code>
      on repository <code>,(build-status-audit-log-full-name self)</code> at ,(%timeago self) ,(if err (format nil ": ~a" err)) </span>))

(defmethod render-audit-log-item ((self t))
  <span> [Missing renderer] Audit log item of type ,(type-of self) </span>)

(defmethod render-audit-log-item ((self access-token-audit-log))
  (let ((err (audit-log-error self)))
      <span class= (if err "text-danger" "") >
    Requested access token for grant type
    <code>,(access-token-audit-log-grant-type self)</code>,
    ,(%timeago self)

    ,(when err
       <span>: ,(progn err)</span>)
    </span>))

(defsettings bitbucket
  :name "bitbucket"
  :title "BitBucket"
  :section :vcs
  :plugin 'bitbucket-plugin
  :handler 'settings-bitbucket-page)

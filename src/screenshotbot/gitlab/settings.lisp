;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/gitlab/settings
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:defsettings
                #:settings-template)
  (:import-from #:screenshotbot/gitlab/plugin
                #:gitlab-plugin)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/user-api
                #:current-user
                #:current-company)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/plugin
                #:plugin-parse-repo)
  (:import-from #:screenshotbot/gitlab/repo
                #:gitlab-repo)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/gitlab/audit-logs
                #:check-personal-access-token
                #:gitlab-audit-log
                #:config-updated-audit-log)
  (:import-from #:screenshotbot/dashboard/audit-log
                #:render-audit-logs)
  (:import-from #:screenshotbot/audit-log
                #:with-audit-log
                #:audit-log-error)
  (:import-from #:alexandria
                #:assoc-value)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/gitlab/settings)

(markup:enable-reader)

(defvar +unchanged+ "unchanged")

(with-class-validation
 (defclass gitlab-settings (store-object)
   ((%company :initarg :company
              :reader company
              :index-type unique-index
              :index-reader gitlab-settings-for-company)
    (url :initarg :url
         :accessor gitlab-url)
    (token :initarg :token
           :accessor gitlab-token
           :documentation "Personal Access Token, to be specific"))
   (:metaclass persistent-class)))

(defmethod plugin-parse-repo ((plugin gitlab-plugin)
                              company
                              repo-str)
  (let ((settings (gitlab-settings-for-company company)))
    (when (and
           settings
           (str:starts-with-p (gitlab-url settings)
                              repo-str))
      (make-instance 'gitlab-repo
                      :company company
                      :link repo-str))))

(defvar *lock* (bt:make-lock))

(defun finish-save-settings (gitlab-url token)
  (let ((company (current-company)))
   (let ((settings (bt:with-lock-held (*lock*)
                     (or
                      (gitlab-settings-for-company company)
                      (make-instance 'gitlab-settings
                                     :company company)))))
     (with-transaction ()
       (setf (gitlab-url settings) gitlab-url)
       (setf (gitlab-token settings) token))
     (make-instance 'config-updated-audit-log
                    :company company
                    :user (current-user))
     (hex:safe-redirect "/settings/gitlab"))))

(defun save-settings (gitlab-url token)
  (let ((errors))
    (flet ((check (key test message)
             (unless test
               (push (cons key message) errors))))
      (check :gitlab-url (not (str:emptyp gitlab-url))
             "GitLab URL cannot be empty")
      (check :token (not (str:emptyp token))
             "Personal Access Token cannot be empty")
      (cond
        (errors
         (with-form-errors (:gitlab-url gitlab-url
                            :token token
                            :errors errors
                            :was-validated t)
           (settings-page)))
        (t
         (finish-save-settings gitlab-url token))))))

(defun test-gitlab-settings ()
  (flet ((settings-page (&rest args)
           (hex:safe-redirect
            (nibble ()
              (apply #'settings-page args)))))
   (with-audit-log (audit-log (make-instance 'check-personal-access-token
                                             :company (current-company)))
     (multiple-value-bind (body code)
         (gitlab-request (current-company)
                         "/personal_access_tokens/self"
                         :ensure-success nil)
       (cond
         ((> code 400)
          (let ((error (format nil "The token appears to be invalid, GitLab responded with ~a" code)))
            (setf (audit-log-error audit-log) error)
            (settings-page :error
                           error)))
         (t
          (let ((json (json:decode-json-from-string body)))
            (let ((scopes (assoc-value json :scopes)))
              (cond
                ((str:s-member scopes "api")
                 (settings-page :success "Access token validated successfully."))
                (t
                 (settings-page :error "The access token does not have the `api` scope.")))))))))))

(defun settings-page (&key error success)
  (let* ((settings (gitlab-settings-for-company (current-company)))
         (current-token (?. gitlab-token settings))
         (save (nibble (gitlab-url token)
                 (let ((token (cond
                                ((equal token +unchanged+)
                                 current-token)
                                (t
                                 token))))
                  (save-settings gitlab-url token))))
         (test (nibble ()
                 (test-gitlab-settings))))
    <settings-template>
      ,(when error
         <div class= "alert alert-danger mt-3">
           ,(progn error)
         </div>)
      ,(when success
         <div class= "alert alert-success mt-3">
           ,(progn success)
         </div>)
      <form action=save method= "POST" >
        <div class= "card mt-3" style= "max-width: 80em">
          <div class= "card-header">
            <h3>GitLab Settings</h3>
          </div>

          <div class= "card-body">
              <div class= "mb-3">
                <label for= "gitlab-url" class= "form-label">GitLab URL</label>
                <input id= "gitlab-url" type= "url" name= "gitlab-url" class= "form-control"
                       value= (or (?. gitlab-url settings ) "https://gitlab.com") />
              </div>

              <div class= "mb-3">
                <label for= "token" class= "form-label">Personal Access Token</label>
                <input id= "token" type= "password" name= "token" class= "form-control"
                       value= (when settings +unchanged+) />
              </div>
          </div>

          <div class= "card-footer">
            <input type= "submit" value= "Save" class= "btn btn-primary"/>
            <a href= test class= "btn btn-secondary">Test settings</a>
          </div>
        </div>
      </form>

      ,(render-audit-logs :type 'gitlab-audit-log
                          :subtitle "All API calls made by Screenshotbot to GitLab in the last 30 days will be listed here.")
    </settings-template>))

(defsettings settings-gitlab-page
  :name "gitlab"
  :section :vcs
  :title "GitLab"
  :plugin 'gitlab-plugin
  :handler 'settings-page)

(defun gitlab-request (repo-or-company url &key (method :get) content
                                             (ensure-success t))
  (let* ((company (if (typep repo-or-company 'company)
                      repo-or-company
                      (company repo-or-company)))
        (settings (gitlab-settings-for-company company)))
    (util/request:http-request
     (format nil "~a/api/v4~a" (gitlab-url settings) url)
     :method method
     :additional-headers `(("PRIVATE-TOKEN" . ,(gitlab-token settings)))
     :want-string t
     :content-type "application/json"
     :ensure-success ensure-success
     :content (json:encode-json-to-string content))))

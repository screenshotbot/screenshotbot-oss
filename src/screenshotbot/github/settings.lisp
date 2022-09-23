;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/github/settings
  (:use #:cl
        #:alexandria
        #:markup
        #:screenshotbot/model/company
        #:screenshotbot/model/github
        #:screenshotbot/user-api
        #:screenshotbot/settings-api)
  (:import-from #:screenshotbot/github/plugin
                #:app-name
                #:github-plugin)
  (:import-from #:screenshotbot/server
                #:staging-p
                #:defhandler
                #:with-login)
  (:import-from
   #:bknr.datastore
   #:with-transaction)
  (:local-nicknames (#:a #:alexandria))
  (:import-from #:screenshotbot/github/webhook
                #:*hooks*)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/github/read-repos
                #:can-edit-repo
                #:read-repo-list)
  (:import-from #:screenshotbot/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:screenshotbot/github/access-checks
                #:repo-string-identifier
                #:github-repo-id)
  (:import-from #:screenshotbot/github/app-installation
                #:app-installed-p)
  (:import-from #:screenshotbot/template
                #:mdi)
  (:import-from #:screenshotbot/github/audit-log
                #:github-audit-logs-for-company)
  (:import-from #:screenshotbot/dashboard/paginated
                #:paginated)
  (:export
   #:verified-repo-p))
(in-package :screenshotbot/github/settings)

(markup:enable-reader)


(defun github-app-installation-callback (state installation-id setup-action)
  (restart-case
      (with-login ()
        (let ((config (github-config (current-company))))
          (cond
            ((str:s-member (list "install" "update") setup-action)
             (with-transaction ()
               (setf (installation-id config)
                     (parse-integer installation-id))))
            (t
             (error "unsupported setup-action: ~S" setup-action))))
        (hex:safe-redirect "/settings/github"))
    (retry-app-installation-callback ()
      (github-app-installation-callback state installation-id setup-action))))


(with-class-validation
  (defclass verified-repo (store-object)
    ((%company :initarg :company
               :reader company
               :index-type hash-index
               :index-reader %verified-repos-for-company)
     (repo-id :initarg :repo-id
              :reader repo-id))
    (:metaclass persistent-class)))

(defun installation-delete-webhook (json)
  (let ((installation (a:assoc-value json :installation)))
   (when (and (equal "deleted" (a:assoc-value json :action))
              installation)
     (let ((id (a:assoc-value installation :id)))
       (delete-installation-by-id id)))))

(defun delete-installation-by-id (id)
  (log:info "Deleting by installation by id: ~a" id)
  (loop for github-config in (class-instances 'github-config)
        if (eql id (installation-id github-config))
          do (with-transaction ()
               (setf (installation-id github-config) nil))))

(pushnew 'installation-delete-webhook
          *hooks*)

(defun render-repo-list (access-token)
  (let ((repos (read-repo-list access-token)))
    <simple-card-page>
      <div class= "card-header">
        <h3>Install the Screenshotbot GitHub app</h3>
      </div>

      <div>
        <p>
          In order for Screenshotbot to be able to post build status (or "
          GitHub Checks") to your pull requests, you need to install the app on your repositories.
        </p>

        <p>
          Below we list all the repositories listed on your account.
        </p>

        <ul>
          ,@ (loop for repo in repos collect
                   <li><a href= (format nil "https://github.com/~a" repo)>,(progn repo)</a></li>)
        </ul>
      </div>

      <div class= "card-footer">
        <a href= "/settings/github" class= "btn btn-secondary" >Done</a>
      </div>
    </simple-card-page>))

(defun verify-repo (repo access-token)
  (let ((errors))
    (flet ((check (field test message)
             (unless test
               (push (cons field message)
                     errors))))
      (check :repo (ignore-errors (github-repo-id repo))
             "Does not look like a valid GitHub repo")
      (check :repo (can-edit-repo access-token repo)
             "You don't seem to have access to this repository. Are you using the wrong GitHub account?")
      (cond
        (errors
         (with-form-errors (:was-validated t
                            :repo repo
                            :errors errors)
           (settings-github-page)))
        (t
         (make-instance 'verified-repo
                         :company (current-company)
                         :repo-id (repo-string-identifier repo))
         (hex:safe-redirect "/settings/github"))))))

(defun verified-repos (company)
  (let ((repos (%verified-repos-for-company company)))
    (let ((table (make-hash-table :test #'equal)))
      (loop for repo in repos do
        (setf (gethash (repo-id repo) table) t))
      (sort
       (a:hash-table-keys table)
       #'string<))))

(defun verified-repo-p (repo company)
  (str:s-member (verified-repos company)
                (repo-string-identifier repo)))

(defun settings-github-page ()
  (let* ((installation-id (installation-id (github-config (current-company))))
         access-token
         (app-configuration-url
           (format nil "https://github.com/apps/~a/installations/new"
                    (app-name (github-plugin))))
         (verify-repo (nibble (repo)
                        (hex:safe-redirect
                         (uiop:call-function
                          ;; TODO: cleanup dependency
                          "screenshotbot/login/github-oauth:make-gh-oauth-link"
                          (uiop:call-function
                           ;; TODO: cleanup dependency
                           "screenshotbot/login/github-oauth:github-oauth-provider")
                          (nibble ()
                            (verify-repo repo access-token))
                          :access-token-callback (lambda (token)
                                                   (setf access-token token))
                          :scope "user:email read:org repo")))))
    <settings-template>
      <div class= "card mt-3" style= "max-width: 80em;" >
        <div class= "card-header">
          <h3>Setup GitHub Checks</h3>
        </div>

        <div class= "card-body" >
          <p>In order to enable GitHub Checks you first need to verify that you have access to the repository, and then install the Screenshotbot app on the repository or organization.</p>

          <form class= "mb-3 mt-3" action=verify-repo method= "POST" >
            <label for= "repo" class= "form-label" >Verify your GitHub repository</label>
            <div class= "input-group" style= "max-width: 50em" >
              <input id= "repo" name= "repo"
                     type= "text" class= "form-control" placeholder= "https://github.com/org/repo" />
              <input type= "submit" class= "btn btn-primary" value= "Verify Repository" />
            </div>
          </form>

        ,(let ((verified-repos (verified-repos (current-company))))
           (cond
             (verified-repos
              <div style="margin-top: 3em" >

                <h4>Verified repositories</h4>
                <table class= "table table-borderless table-hover" >
                  <thead>
                    <tr>
                      <th>Repository</th>
                      <th>Status</th>
                      <th>Actions</th>
                    </tr>
                  </thead>
                  <tbody>

                  ,@ (loop for repo in verified-repos
                           for app-installed-p = (app-installed-p repo)
                           collect
                               <tr>
                                 <td>
                                   ,(progn repo)
                                 </td>
                                   ,(cond
                                      (app-installed-p
                                       <markup:merge-tag>
                                         <td>
                                           <span>
                                             <span class= "text-success">
                                               <mdi name= "done" /> Verified, and App is installed
                                             </span>
                                           </span>
                                         </td>
                                         <td>
                                           <a href= app-configuration-url >
                                             Configure on GitHub
                                           </a>
                                         </td>
                                       </markup:merge-tag>
                                       )
                                      (t
                                       <markup:merge-tag>
                                         <td>
                                           <span class= "text-danger">
                                             <mdi name= "error" />
                                             Verified, but app not installed
                                           </span>
                                         </td>
                                         <td>
                                           <a href= app-configuration-url >Install App</a>
                                         </td>
                                       </markup:merge-tag>))
                               </tr>)
                             </tbody>
                           </table>
              </div>)))



          <div class= "alert alert-info" >
            The GitHub app does <b>not</b> get permissions to access to your repositories, it only needs write access to the Checks API. We will request access to read repository metadata only during the verification step.
          </div>
        </div>

        <div class= "card-footer">

          <a href= app-configuration-url
             class= (if installation-id "btn btn-outline-secondary" "btn btn-outline-primary") >
            ,(if installation-id
                 "Configure"
                 "Install App on GitHub")
          </a>
        </div>
      </div>

      ,(when (staging-p)
         (render-audit-logs))
    </settings-template>))

(defmethod render-audit-log-item ((self t))
  <span> [Missing renderer] Audit log item of type ,(type-of self) </span>)

(deftag render-audit-logs ()
  <div class= "card mt-3 audit-log-card" style= "max-width: 80em" >
    <div class= "card-header">
      <h5>API Audit Logs</h5>
    </div>

  <div class= "card-body">
    <p class= "text-muted">All API calls to GitHub made by Screenshotbot in the last 30 days will be listed here. This does not include OAuth calls since that's made on the user's behalf.</p>

    <ul>
      ,(paginated
        (lambda (x)
          <li>,(render-audit-log-item x)</li>)
        :items (github-audit-logs-for-company (current-company)))
    </ul>
  </div>
  </div>)

(defsettings settings-github-page
  :name "github"
  :section :vcs
  :title "GitHub"
  :plugin 'github-plugin
  :handler 'settings-github-page)


(defhandler (nil :uri "/github-app-install-callback") (state installation_id setup_action)
  (github-app-installation-callback state installation_id setup_action))

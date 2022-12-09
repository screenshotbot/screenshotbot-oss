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
                #:repo-collaborator-p
                #:whoami
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
                #:get-repo-id
                #:repo-string-identifier
                #:github-repo-id)
  (:import-from #:screenshotbot/github/app-installation
                #:app-installed-p)
  (:import-from #:screenshotbot/template
                #:mdi)
  (:import-from #:screenshotbot/github/audit-log
                #:github-audit-log
                #:github-audit-logs-for-company)
  (:import-from #:screenshotbot/dashboard/paginated
                #:paginated)
  (:import-from #:screenshotbot/dashboard/audit-log
                #:render-audit-logs)
  (:import-from #:screenshotbot/ui/confirmation-page
                #:confirmation-page)
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
              :reader repo-id)
     (installer-login :initarg :installer-login
                      :reader installer-login
                      :documentation "The github login (e.g. tdrhq) of the person who installed this repo on
 Screenshotbot. This is used later to verify that the app is installed by this person.")
     (verified-p :initform nil
                 :accessor verified-p)
     (verification-failure-message :initform nil
                                   :accessor verification-failure-message))
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



(deftag refresh ()
  <a href= "javascript:window.location.reload()">Refresh</a>)

(defun verify-repo (repo access-token)
  (let ((errors))
    (flet ((check (field test message)
             (unless test
               (push (cons field message)
                     errors))))
      (check :repo
             (and
              (cl-ppcre:scan "https://github.com/.*/.*" repo)
              (ignore-errors (get-repo-id repo)))
             "Does not look like a valid GitHub repo")
      (check :repo
             (not (str:s-member (verified-repos (current-company)) (get-repo-id repo)))
             "We're already tracking a repo with that ID")

      (cond
        (errors
         (with-form-errors (:was-validated t
                            :repo repo
                            :errors errors)
           (settings-github-page)))
        (t
         (let ((verified-repo
                 (make-instance 'verified-repo
                                :installer-login (whoami access-token)
                                :company (current-company)
                                :repo-id (repo-string-identifier repo))))
           (maybe-verify-repo verified-repo :access-token access-token))
         (hex:safe-redirect "/settings/github"))))))

(defmethod maybe-verify-repo ((self verified-repo) &key access-token)
  (multiple-value-bind (can-edit-p message)
      (repo-collaborator-p (format nil "https://github.com/~a" (repo-id self))
                           (installer-login self)
                           :access-token access-token
                           :company (company self))
    (cond
      (can-edit-p
       (with-transaction ()
         (setf (verified-p self) t
               (verification-failure-message self) nil)))
      ((not (verified-p self))
       (with-transaction ()
         (setf (verification-failure-message self) message))))))

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

(defun remove-verification (verified-repo)
  (let ((redirect "/settings/github"))
   (confirmation-page
    :yes (nibble ()
           (bknr.datastore:delete-object verified-repo)
           (hex:safe-redirect redirect))
    :no redirect
    <div>
      <p>Removing verification does <b>not</b> uninstall the GitHub app.</p>

      <p>Removing the verification prevents Screenshotbot from sending build statuses to GitHub.</p>

      <p>Are you sure you want to remove this verification?</p>
    </div>)))

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

        ,(let ((verified-repos (%verified-repos-for-company (current-company))))
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
                           for app-installed-p = (app-installed-p (repo-id repo))
                           collect
                           (util:copying (repo)
                             (let ((remove-verification (nibble ()
                                                          (remove-verification repo))))
                               <tr>
                                 <td>
                                   ,(repo-id repo)
                                 </td>
                                 ,(cond
                                    ((and
                                      app-installed-p
                                      (verified-p repo))
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
                                         ,(progn "|")
                                         <a href= remove-verification >Remove</a>
                                       </td>
                                     </markup:merge-tag>)
                                    ((and
                                      app-installed-p
                                      (not (verified-p repo)))
                                     <markup:merge-tag>
                                       <td>
                                         <span>
                                           <span class= "text-danger">
                                             <mdi name= "error" />Could not verify
                                             (GitHub said: ,(verification-failure-message repo))
                                           </span>
                                         </span>
                                       </td>
                                       <td>
                                         <a href= app-configuration-url >
                                           Configure on GitHub
                                         </a>
                                         ,(progn "|")
                                         <a href= remove-verification >Remove</a>
                                       </td>
                                     </markup:merge-tag>)
                                    (t ;; (not app-installed-p)
                                     <markup:merge-tag>
                                       <td>
                                         <span class= "text-danger">
                                           <mdi name= "error" />
                                           App not installed
                                           <refresh />

                                         </span>
                                       </td>
                                       <td>
                                         <a href= app-configuration-url >Install App</a>
                                         ,(progn "|")
                                         <a href= remove-verification >Remove</a>
                                       </td>
                                     </markup:merge-tag>))
                               </tr>)))

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

      ,(render-audit-logs
        :type 'github-audit-log
        :subtitle "All API calls to GitHub made by Screenshotbot in the last 30 days will be listed here. This does not include OAuth calls since that's made on the user's behalf.")
    </settings-template>))


(defsettings settings-github-page
  :name "github"
  :section :vcs
  :title "GitHub"
  :plugin 'github-plugin
  :handler 'settings-github-page)


(defhandler (nil :uri "/github-app-install-callback") (state installation_id setup_action)
  (github-app-installation-callback state installation_id setup_action))

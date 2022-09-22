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
                #:defhandler
                #:with-login)
  (:import-from
   #:bknr.datastore
   #:with-transaction))
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

(defun settings-github-page ()
  (let ((installation-id (installation-id (github-config (current-company)))))
    <settings-template>
      <div class= "card mt-3">
        <div class= "card-header">
          <h3>Setup GitHub Checks</h3>
        </div>

        <div class= "card-body">
          <p>In order to enable Build Statuses (called GitHub Checks) you will need to install the Screenshotbot Checks app to your GitHub organization.</p>

          <p>
            This app does <b>not</b> get permissions to access to your repositories, it only needs write access to the Checks API.
          </p>
        </div>

        <div class= "card-footer">

          <a href= (format nil "https://github.com/apps/~a/installations/new"
                    (app-name (github-plugin)))
             class= (if installation-id "btn btn-secondary" "btn btn-primary") >
            ,(if installation-id
                 "Configure"
                 "Install App on GitHub")
          </a>
        </div>
      </div>
    </settings-template>))

(defsettings settings-github-page
  :name "github"
  :section :vcs
  :title "GitHub"
  :plugin 'github-plugin
  :handler 'settings-github-page)


(defhandler (nil :uri "/github-app-install-callback") (state installation_id setup_action)
  (github-app-installation-callback state installation_id setup_action))

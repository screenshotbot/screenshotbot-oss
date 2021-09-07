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
            ((equal "install" setup-action)
             (with-transaction ()
               (setf (installation-id config)
                     (parse-integer installation-id))))
            (t
             (error "unsupported setup-action: ~S" setup-action))))
        (hex:safe-redirect "/settings/github"))
    (retry-app-installation-callback ()
      (github-app-installation-callback state installation-id setup-action))))

(defun settings-github-page ()
  <settings-template>
    <div class= "card mt-3">
      <div class= "card-header">
        <h3>Setup GitHub Checks</h3>
      </div>

      <div class= "card-body">
        <p>In order to enable Build Statuses (called GitHub Checks) we need you to install the Screenshotbot Checks app to your GitHub organization.</p>

        <p>Please contact us if you are using an Enterprise on-prem GitHub, and we can enable this for you.</p>
      </div>

      <div class= "card-footer">
        <a href= (format nil "https://github.com/apps/screenshotbot/installations/new")
           class= "btn btn-primary" >
          Install App on GitHub
        </a>
      </div>
    </div>
  </settings-template>)

(defsettings settings-github-page
  :name "github"
  :section :vcs
  :title "GitHub"
  :plugin 'github-plugin
  :handler 'settings-github-page)


(defhandler (nil :uri "/github-app-install-callback") (state installation_id setup_action)
  (github-app-installation-callback state installation_id setup_action))

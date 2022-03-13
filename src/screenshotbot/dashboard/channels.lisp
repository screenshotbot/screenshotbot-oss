;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/channels
  (:use #:cl
        #:alexandria
        #:screenshotbot/template
        #:screenshotbot/user-api
        #:screenshotbot/taskie
        #:nibble
        #:screenshotbot/ui)
  (:import-from #:screenshotbot/server
                #:with-login
                #:defhandler)
  (:import-from #:markup #:deftag))
(in-package :screenshotbot/dashboard/channels)

(markup:enable-reader)

(defun single-channel-view (channel)
  <simple-card-page max-width="60em" >
    <div class= "card-header">
      <h3>,(channel-name channel)</h3>
    </div>
      <p>First seen: <timeago timestamp= (created-at channel) />
      </p>
      <p>Active run:
        ,(anaphora:acase (channel-active-run channel)
        (nil
        "None"
        (otherwise
        <a href=(make-url 'run-page :id (oid it)) >
          Run
      </a>)))
      </p>
  </simple-card-page>)

(deftag channel-list-row (&key channel)
  (taskie-row :object channel
              (ui/a :href (nibble ()
                            (single-channel-view channel))
                    (channel-name channel))
              (ui/div)
              (taskie-timestamp :prefix "First created"
                                :timestamp (created-at channel))))

(defun %list-projects (&key
                         (user (current-user))
                         (company (current-company)))
    (let ((channels (sort (company-channels company)
                        '|STRING<| :key 'channel-name)))
    (with-pagination (channels channels :next-link next-link :prev-link prev-link)
      (dashboard-template :user user :company company :script-name "/projects"
         (taskie-page-title :title "Project List")
        (taskie-list :empty-message "No projects to show! Projects are
                                   automatically created when you start a run"
                     :items channels
                     :next-link next-link
                     :prev-link prev-link
                     :row-generator (lambda (channel)
                                 (channel-list-row :channel channel)))))))

(defhandler (projects-page :uri "/projects") ()
  (with-login ()
    (%list-projects)))

(defhandler (channels-page :uri "/channels") ()
  (hex:safe-redirect 'projects-page))

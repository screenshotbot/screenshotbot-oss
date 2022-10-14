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
  (:import-from #:markup #:deftag)
  (:import-from #:screenshotbot/dashboard/explain
                #:explain)
  (:import-from #:screenshotbot/model/channel
                #:all-active-runs)
  (:import-from #:util/object-id
                #:oid)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-page)
  (:import-from #:screenshotbot/taskie
                #:taskie-page-title))
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
        ,@ (or
            (loop for (branch . run) in (all-active-runs channel)
                  collect
                  <a href=(hex:make-url 'run-page :id (oid run)) >
                    Run on ,(progn branch)
                  </a>)
            (list "None"))
      </p>
  </simple-card-page>)

(deftag channel-list-row (&key channel)
  (taskie-row :object channel
              (ui/a :href (nibble ()
                            (single-channel-view channel))
                    (channel-name channel))
              (taskie-timestamp :timestamp (created-at channel))))

(deftag explain-channels ()
  <div>
    <p>A <b>channel</b> is a name used to track a collection of screenshots.</p>

    <p>Channel names are arbitrary. It could refer to build targets in your repository, or maybe <code>staging</code> or <code>production</code>, or just your repository name on GitHub.</p>

    <p>You don't have to explicitly create a channel before you use it. Calling the SDK with <code>--channel</code> will automatically create the channel for you.</p>

    <p>Web projects will also be associated with a channel by the same name as the project.</p>
  </div>)

(defun channel-page-title ()
  <span>
    Channel List <explain title= "Channels">,(explain-channels)</explain>
  </span>)

(defun %list-projects (&key
                         (user (current-user))
                         (company (current-company)))
    (let ((channels (sort (company-channels company)
                        '|STRING<| :key 'channel-name)))
    (with-pagination (channels channels :next-link next-link :prev-link prev-link)
      (dashboard-template :user user :company company :script-name "/channels" :title "Screenshotbot: Channels"
       <taskie-page-title title= (channel-page-title) >
       </taskie-page-title>

        (taskie-list :empty-message "No projects to show! Projects are
                                   automatically created when you start a run"
                     :items channels
       :headers (list "Channel" "First created")
                     :next-link next-link
       :checkboxes nil
                     :prev-link prev-link
                     :row-generator (lambda (channel)
                                 (channel-list-row :channel channel)))))))



(defhandler (projects-page :uri "/channels") ()
  (with-login ()
    (%list-projects)))

(defhandler (channels-page :uri "/projects") ()
  (hex:safe-redirect 'projects-page))

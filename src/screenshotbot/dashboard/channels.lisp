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
                #:channel-company
                #:all-active-runs)
  (:import-from #:util/object-id
                #:find-by-oid
                #:oid)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-page)
  (:import-from #:screenshotbot/taskie
                #:taskie-page-title)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-screenshots
                #:current-company
                #:company-channels)
  (:import-from #:screenshotbot/model/recorder-run
                #:active-run)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:screenshotbot/ui/simple-card-page
                #:simple-card-page))
(in-package :screenshotbot/dashboard/channels)

(markup:enable-reader)

(defun single-channel-view (channel)
  <app-template >
    <div class= "main-content">
      <div class= "card-page-container mt-3 mx-auto" style= "max-width: 60em" >
        <div class= "card">
          <div class= "card-header">
            <h3>,(channel-name channel)</h3>
          </div>
          <div class= "card-body">
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
          </div>
        </div>

        <div class= "card mt-3">
          <div class= "card-header">
            <h3>Build Badge</h3>
          </div>

          <div class= "card-body">
            ,(let* ((args (guess-channel-args channel))
                    (link (apply #'hex:make-full-url
                                 hunchentoot:*request*
                                 'channel-static-handler
                                 args))
                    (badge (apply #'hex:make-full-url
                                  hunchentoot:*request*
                                  'badge-handler
                                  args))
                    (local-badge
                      ;; A local-badge is better for screenshot tests
                      (apply #'hex:make-url
                             'badge-handler
                             args)))
               <markup:merge-tag>
                 <a href=link >
                   <img src= local-badge />
                 </a>

                 <p class= "mt-3" >To use this build badge, you can use the following template in a GitHub flavored markdown file. </p>


                 <div>
                   <tt>
                     [![Screenshots](,(progn badge))](,(progn link))
                   </tt>
                 </div>

                 <div class= "alert alert-warning mt-3">
                   The badge works on private channels only if you are logged in, but
                   will always work on public channel. GitHub renders
                   the badges behind a proxy which might break the badges for
                   private channels.
                   Reach out to us if the badge doesn't render,
                   and we'll help you fix it.
                 </div>
               </markup:merge-tag>)

          </div>
        </div>

      </div>
    </div>
  </app-template>)

(defun guess-channel-args (channel)
  (list
   :org (oid (channel-company channel))
   :channel (channel-name channel)
   :branch (caar (all-active-runs channel))))

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

(defun run-for-channel (&key channel company branch)
  "Use this for external links that need to reference a specific channel.
 We'll return the based run for the given arguments"
  (let ((company (if (typep company 'company)
                     company
                     (find-by-oid company))))
    (check-type company company)
    (let ((channel (loop for c in (company-channels company)
                         if (string-equal channel (channel-name c))
                           return c)))
      (let ((run (active-run channel branch)))
        (when run
          (can-view! run)
          run)))))

(defvar *badge-cache* (make-hash-table :test #'equal))

(defun badge-data (&key label message color)
  (util:or-setf
   (gethash (list :v1 label message color) *badge-cache*)
   ;; TODO: use something like
   ;; https://github.com/dsibilio/badge-maker. Easier to unit test
   ;; that way.
   (let ((url "https://img.shields.io/static/v1"))
     (http-request
      url
      :parameters `(("label" . ,label)
                    ("message" . ,message)
                    ("color" . ,color))
      :accept "image/svg+xml"
      :want-string t))))

(defhandler (badge-handler :uri "/badge") (org channel branch)
  (let ((run (run-for-channel
              :channel channel
              :company (or org (current-company))
              :branch branch)))
    (let ((data (badge-data
                 :label "Screenshotbot"
                 :message (if run (format nil "~a screenshots" (length (recorder-run-screenshots run)))
                              "No active run for parameters")
                 :color (if run "green" "red"))))
      (setf (hunchentoot:content-type*) "image/svg+xml")
      data)))

(defhandler (channel-static-handler :uri "/active-run") (org channel branch)
  (let ((run (run-for-channel
              :channel channel
              :company (or org (current-company))
              :branch branch)))
    (cond
      ((not run)
       <simple-card-page>
         <div class= "card-body">
           No such channel or no active runs for this channel.
         </div>
       </simple-card-page>)
      (t
       (can-view! run)
       (hex:safe-redirect 'run-page :id (oid run))))))

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

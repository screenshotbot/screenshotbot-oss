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
                #:channel-slack-channels
                #:channel-subscribers
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
                #:company-admin-p
                #:company)
  (:import-from #:screenshotbot/user-api
                #:can-view!
                #:recorder-run-channel
                #:company-runs
                #:recorder-run-screenshots
                #:current-company
                #:company-channels)
  (:import-from #:screenshotbot/model/recorder-run
                #:active-run)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:core/ui/simple-card-page
                #:confirmation-page
                #:simple-card-page)
  (:import-from #:util/misc
                #:make-mp-hash-table)
  (:import-from #:bknr.datastore
                #:deftransaction)
  (:import-from #:bknr.datastore
                #:deftransaction)
  (:import-from #:screenshotbot/model/report
                #:report-channel)
  (:import-from #:alexandria
                #:curry
                #:removef)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:bknr.datastore
                #:store-object-with-id)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:util/form-errors
                #:with-error-builder))
(in-package :screenshotbot/dashboard/channels)

(named-readtables:in-readtable markup:syntax)


(deftag subscription-card (&key channel)
  (let ((subscribe (nibble (:method :post)
                     (with-transaction ()
                       (pushnew (current-user)
                                (channel-subscribers channel)))
                     (go-back channel)))
        (subscribedp (member (current-user) (channel-subscribers channel)))
        (unsubscribe (nibble (:method :post)
                       (with-transaction ()
                         (removef (channel-subscribers channel)
                                  (current-user)))
                       (go-back channel))))
    <div class= "card mt-3">
      <div class= "card-body">

        ,(cond
           (subscribedp
            <markup:merge-tag>
              <p>You are subscribed to this channel, and will always get notified by email when then screenshots change on the main branch.</p>
              <form action=unsubscribe method= "post">
                <input type= "submit" class= "btn btn-warning" value= "Unsubscribe" />
              </form>
            </markup:merge-tag>)
           (t
            <markup:merge-tag>
              <p>You are <b>not</b> subscribed to this channel, and might not get notified when screenshots change on the main branch.</p>

              <form action=subscribe method= "post">
                <input type= "submit" class= "btn btn-primary" value = "Subscribe" />
              </form>
            </markup:merge-tag>))

      </div>
    </div>))

(deftag slack-card (&key channel)
  (let ((slack-update (nibble (slack-channels :method :post)
                        (slack-card-post channel slack-channels)))
        (slack-channels (str:join ", "
                           (mapcar (curry #'str:concat "#")
                           (channel-slack-channels channel)))))
    <div class= "card mt-3">
      <div class= "card-body">
        <h4 class= "card-title">
          Notify on Slack
        </h4>
        <p class= "text-muted">If you have configured <a href= "/settings/slack">Slack</a>, these Slack channels will always be notified of <tt>,(channel-name channel)</tt>'s changes.</p>

        <form method= "POST" action=slack-update >
          <div class= "input-group">
            <input name= "slack-channels" class= "form-control"
                   value= slack-channels
                   placeholder= "#channel1, #channel2" />
            <input type= "submit" class= "btn btn-primary" value= "Save" />
          </div>
        </form>
      </div>
    </div>))

(defun slack-card-post (channel slack-channels)
  (with-error-builder (:check check :errors errors)
    (flet ((parse-channel (channel)
             (let ((channel (str:trim channel)))
               (if (str:starts-with-p "#" channel)
                   (str:substring 1 nil channel)
                   channel))))
      (let ((slack-channels (remove-if #'str:emptyp
                                       (mapcar #'str:trim
                                               (mapcar (curry #'str:replace-all "#" "")
                                                (str:split "," slack-channels))))))
        (assert (< (length slack-channels) 100))
        (with-transaction ()
          (setf (channel-slack-channels channel)
                slack-channels)))
      (go-back channel))))

(defhandler (single-channel-page :uri "/channels/:id") (id)
  (let* ((id (parse-integer id))
         (channel (store-object-with-id id)))
    (check-type channel channel)
    (can-view! channel)
    (single-channel-view channel)))

(defun go-back (channel)
  (hex:safe-redirect
   (hex:make-url
    'single-channel-page :id (store-object-id channel))))

(defun single-channel-view (channel)
  <app-template >
    <div class= "main-content">
      <div class= "card-page-container mt-3 mx-auto" style= "max-width: 60em" >
        <div class= "card">
          <div class= "card-header d-flex justify-content-between">
            <h3>,(channel-name channel)</h3>
            <a href= (nibble () (confirm-delete channel)) class= "btn btn-danger">Delete</a>
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


        <subscription-card channel=channel />

        ,(when (gk:check :slack-subscription (current-company) :default nil)
           <slack-card channel=channel />)

        <div class= "card mt-3">
          <div class= "card-body">
            <h4 class= "card-title mb-2" >Build Badge</h4>

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
                   <:img src= local-badge />
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

(defun confirm-delete (channel)
  (cond
    ((company-admin-p (channel-company channel)
                      (current-user))
     (confirmation-page
      :yes (nibble ()
             (perform-delete channel)
             (channel-deleted-confirmation))
      :no (nibble ()
            (simple-confirmation-page channel))
      :danger t
      <span>Deleting this channel will delete all associated runs and reports. This cannot be
        undone. Are you sure you want to continue?</span>))
    (t
     <simple-card-page>
       <span>You must be a company admin to delete channels.</span>
     </simple-card-page>)))

(defun channel-deleted-confirmation ()
  <simple-card-page>
    <span>Channel deleted. <a href= "/channels">Back to channels</a></span>
  </simple-card-page>)

(deftransaction perform-delete (channel)
  (check-type channel channel)
  ;; Deleting a channel is an expensive operation, and we're blocking all transactions
  ;; in the meantime.
  (let ((company (company channel)))
    (setf (company-runs company)
          (remove channel (company-runs company)
                  :key #'recorder-run-channel))
    (setf (company-reports company)
          (remove channel (company-reports company)
                  :key #'report-channel))
    (setf (company-channels company)
          (remove channel (company-channels company)))

    (setf (company channel) nil)))

(defun guess-channel-args (channel)
  (list
   :org (oid (channel-company channel))
   :channel (channel-name channel)
   :branch (caar (all-active-runs channel))))

(deftag channel-list-row (&key channel)
  (taskie-row :object channel
              (ui/a :href (hex:make-url
                           'single-channel-page
                           :id (store-object-id channel))
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

(defvar *badge-cache* (make-mp-hash-table :test #'equal))

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
      (setf (hunchentoot:header-out :cache-control) "max-age=600")
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

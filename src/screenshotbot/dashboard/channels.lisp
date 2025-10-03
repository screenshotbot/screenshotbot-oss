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
        #:core/ui/taskie
        #:nibble
        #:screenshotbot/ui)
  (:import-from #:screenshotbot/server
                #:with-login
                #:defhandler)
  (:import-from #:markup #:deftag)
  (:import-from #:screenshotbot/dashboard/explain
                #:explain)
  (:import-from #:screenshotbot/model/channel
                #:channel-masks
                #:allow-public-badge-p
                #:review-policy-name
                #:with-channel-lock
                #:channel-slack-channels
                #:channel-subscribers
                #:channel-company
                #:all-active-runs)
  (:import-from #:util/object-id
                #:find-by-oid
                #:oid)
  (:import-from #:screenshotbot/dashboard/run-page
                #:mask-editor
                #:run-page)
  (:import-from #:core/ui/taskie
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
                #:runs-for-channel
                #:delete-run
                #:recorder-run-company
                #:active-run)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:core/ui/simple-card-page
                #:confirmation-page
                #:simple-card-page)
  (:import-from #:util/misc
                #:make-mp-hash-table)
  (:import-from #:bknr.datastore
                #:store-object-id
                #:without-sync
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
                #:with-error-builder)
  (:import-from #:screenshotbot/dashboard/recent-runs
                #:render-recent-runs)
  (:import-from #:core/ui/mdi
                #:mdi)
  (:import-from #:screenshotbot/dashboard/flaky-screenshots
                #:view-noisy-screenshots
                #:view-flaky-screenshots)
  (:import-from #:util/copying
                #:copying)
  (:export
   #:microsoft-teams-card))
(in-package :screenshotbot/dashboard/channels)

(named-readtables:in-readtable markup:syntax)


(defgeneric microsoft-teams-card (channel))

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

(deftag settings-card (&key channel)
  (let ((slack-update (nibble (slack-channels review-policy allow-public-badge-p :method :post)
                        (settings-card-post channel
                                            :slack-channels slack-channels
                                            :allow-public-badge-p allow-public-badge-p
                                            :review-policy review-policy)))
        (slack-channels (str:join ", "
                           (mapcar (curry #'str:concat "#")
                           (channel-slack-channels channel)))))
    <div class= "card mt-3">
      <div class= "card-body">
        <h4 class= "card-title mb-3">
          Settings
        </h4>


        <form method= "POST" action=slack-update >
          <div class= "mb-3">
            <label for= "slack-channels" class= "form-label">Slack channels to notify
              <explain>
                If you have configured <a href= "/settings/slack">Slack</a>, these Slack channels will always be notified of <tt>,(safe-channel-name channel)</tt>'s changes.
              </explain>
            </label>
            <input name= "slack-channels" class= "form-control"
                   id= "slack-channels"
                   value= slack-channels
                   placeholder= "#channel1, #channel2" />
<div class= "mt-1 mb-2 d-none" class= "text-muted">If you have configured <a href= "/settings/slack">Slack</a>, these Slack channels will always be notified of <tt>,(safe-channel-name channel)</tt>'s changes.</div>

          </div>

          <div class= "mb-3" >
            <label for= "review-policy" class= "form-label" >Review policy
              <explain>
                This setting only affects screenshot on Pull Requests or Merge Requests. Our
                recommendation is to allow authors to review their own screenshots.
              </explain>
            </label>
            ,(flet ((selected (name)
                      (when (string-equal name (string (review-policy-name channel)))
                        "selected")))
               <select class="form-select" id= "review-policy" name= "review-policy" >
                 <option value="allow-author" selected= (selected "allow-author") >Authors can accept their own changes (Default) </option>
                 <option value="disallow-author" selected= (selected "disallow-author") >Authors cannot accept their own changes</option>
               </select>)
          </div>

          <div class= "form-check mb-3">
            <input class= "form-check-input" type= "checkbox"   id= "allowPublicBadge"
                   name= "allow-public-badge-p"
                   checked= (when (allow-public-badge-p channel) "checked") />
            <label for= "allowPublicBadge" class= "form-check-label">Allow public Build Badge for private repositories</label>
            <explain>
              By default the Build Badge is only accessible to logged-in users, unless the
              repository is public. Enabling public access will be required before using the build
              badge on a private repository.
            </explain>
          </div>

          <input type= "submit" class= "btn btn-primary" value= "Save" />
        </form>
      </div>
    </div>))

(defun settings-card-post (channel &key slack-channels review-policy allow-public-badge-p)
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

        (assert (str:s-member '("allow-author" "disallow-author")
                              review-policy))
        (assert (< (length slack-channels) 100))

        (setf (channel-slack-channels channel)
              slack-channels)
        (setf (review-policy-name channel)
              (find-symbol (str:upcase review-policy) :keyword))
        (setf (allow-public-badge-p channel)
              allow-public-badge-p))
      (go-back channel))))

(defhandler (single-channel-page :uri "/channels/:id") (id)
  (with-login ()
    (let* ((id (parse-integer id))
           (channel (store-object-with-id id)))
      (check-type channel channel)
      (can-view! channel)
      (single-channel-view channel))))

(defun go-back (channel)
  (hex:safe-redirect
   (hex:make-url
    'single-channel-page :id (store-object-id channel))))

(defun view-channel-runs (channel)
  (render-recent-runs (runs-for-channel channel)
                      :title (format nil "Runs for ~a" (channel-name channel))))

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


(defun single-channel-view (channel)
  <app-template >
    <div class= "main-content channel-view">
      <div class= "card-page-container mt-3 mx-auto" style= "max-width: 60em" >
        <div class= "card">
          <div class= "card-header d-flex justify-content-between">
            <h3>,(safe-channel-name channel)</h3>
            <div>
              <a href= (nibble () (confirm-delete channel)) class= "btn btn-danger ms-2"  >Delete</a>
            </div>
          </div>
          <div class= "card-body">
            <p>First seen: <timeago timestamp= (created-at channel) />
            </p>
            <p>
              <ul class= "channel-links" >
                ,@ (or
                    (loop for (branch . nil) in (all-active-runs channel)
                          collect
                          <li>
                            <a href=(hex:make-url 'channel-static-handler
                                                  :org (oid (company channel))
                                                  :channel (channel-name channel)
                                                  :branch branch) >
                              <mdi name= "image" />
                              View promoted screenshots on <tt>,(progn branch)</tt>
                            </a>
                          </li>)
                    (list <li>
                            <mdi name= "image" />
                            No promoted screenshots
                          </li>))
                 <li>
                   <a href= (nibble () (view-channel-runs channel)) >
                     <mdi name= "view_list" />
                     View recent runs
                   </a>
                 </li>

                 <li>
                   <a href= (nibble () (view-noisy-screenshots channel)) >
                     <mdi name= "flaky" />
                     Debug flaky screenshots
                   </a>
                 </li>

                 <li>
                   <a href= (nibble () (view-masks channel)) >
                     <mdi name= "masks" />
                     View masks
                   </a>
                 </li>
                          
              </ul>
            </p>
          </div>
        </div>


        <subscription-card channel=channel />

        <settings-card channel=channel />

        ,(when (gk:check :microsoft-teams (auth:current-company))
           (microsoft-teams-card channel))
        

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

(defun view-masks (channel)
  (let ((run (fset:greatest (runs-for-channel channel)))
        (mask-map (loop for (name . masks) in (channel-masks channel)
                        if masks
                          collect (cons name masks))))
    (cond
      ((not mask-map)
       <simple-card-page>
         <span>No masks are set for this channel</span>
       </simple-card-page>)
      ((not run)
       <simple-card-page>
         <span>No runs available for this channel</span>
       </simple-card-page>)
      (t
       <app-template>
         <h4 class= "mt-3" >All masks for ,(channel-name channel)</h4>
         <table class= "table table-striped" >
         ,@ (loop for (name . masks) in mask-map
                  collect
                  (copying (name)
                    <tr>
                      <td>
                        <div>
                          <a href= (nibble () (%edit-masks-for-screenshot run name)) >,(progn name)</a>
                        </div>
                      </td>
                      <td>
                        ,(length masks) masks
                      </td>
                    </tr>))
         </table>
       </app-template>))))

(defun %delete-mask (channel name)
  (setf
   (channel-masks channel)
   (remove name (channel-masks channel)
           :test #'equal
           :key #'car))
  (hex:safe-redirect (nibble () (view-masks channel))))

(defmethod %edit-masks-for-screenshot (run (name string))
  (let* ((screenshots (recorder-run-screenshots run))
         (channel (recorder-run-channel run))
         (screenshot (loop for screenshot in screenshots
                           if (equal name (screenshot-name screenshot))
                             return screenshot))
         (delete-action (nibble ()
                          (%delete-mask channel name))))

    (cond
      ((not screenshot)
       <simple-card-page form-action=delete-action >
         <span>This screenshot is no longer active and the mask cannot be edited</span>
         <div class= "card-footer">
           <input type= "submit" class= "btn btn-danger" value= "Delete Mask" />
         </div>
       </simple-card-page>)
      (t
       (mask-editor channel screenshot :redirect (format nil "/channels/~a" (store-object-id channel)))))))

(defun confirm-delete (channel)
  (cond
    ((company-admin-p (channel-company channel)
                      (current-user))
     (confirmation-page
      :yes (nibble ()
             (perform-delete channel)
             (channel-deleted-confirmation))
      :no (nibble ()
            (go-back channel))
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

(defun perform-delete (channel)
  (check-type channel channel)
  (without-sync ()
   (with-channel-lock (channel)
     (let ((company (company channel)))
       (assert company)
       (with-transaction ()
         (setf (company-reports company)
               (remove channel (company-reports company)
                       :key #'report-channel)))
       (fset:do-set (run (runs-for-channel channel))
         (delete-run run))
       (with-transaction ()
         (setf (company-channels company)
               (remove channel (company-channels company))))
       (with-transaction ()
         (setf (company channel) nil))))))


(defun guess-channel-args (channel)
  (list
   :org (oid (channel-company channel))
   :channel (channel-name channel)
   :branch (caar (all-active-runs channel))))

(defun safe-channel-name (channel)
  (let ((name (channel-name channel)))
    (if (str:emptyp name)
        <em>empty</em>
        name)))

(deftag channel-list-row (&key channel)
  <taskie-row object=channel >
    <a href=(hex:make-url
             'single-channel-page
             :id (store-object-id channel))>
      ,(safe-channel-name channel)
    </a>
    ,(taskie-timestamp :timestamp (created-at channel))
  </taskie-row>)

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
        run))))

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
    (when run
     (unless (allow-public-badge-p (recorder-run-channel run))
       (auth:can-view! run)))
    
    (let ((data (badge-data
                 :label "Screenshotbot"
                 :message (if run (format nil "~a screenshots" (length (recorder-run-screenshots run)))
                              "No active run for parameters")
                 :color (if run "green" "red"))))
      (setf (hunchentoot:header-out :cache-control) "max-age=600")
      (setf (hunchentoot:content-type*) "image/svg+xml")
      data)))

(defhandler (badge-handler-svg :uri "/badge.svg") (org channel branch)
  (badge-handler :org org :channel channel :branch branch))

(defun %render-channels-as-taskie (channels &key next-link prev-link)
  (taskie-list :empty-message "No projects to show! Projects are
                                   automatically created when you start a run"
               :items channels
               :headers (list "Channel" "First created")
               :next-link next-link
               :checkboxes nil
               :prev-link prev-link
               :row-generator (lambda (channel)
                                (channel-list-row :channel channel))))

(defun %render-channels-for-search-query (company query)
  (let* ((query (str:downcase query))
         (channels (company-channels company))
         (channels (loop for channel in channels
                         if (str:containsp query (str:downcase (channel-name channel)))
                           collect channel)))
    (%render-channels-as-taskie
     (loop for channel in channels
           for i from 0 below 200
           collect channel))))

(defun %list-projects (&key
                         (user (current-user))
                         (company (current-company)))
  (auth:can-view! company)
  (let ((channel-search (nibble (search)
                          (markup:write-html
                           (%render-channels-for-search-query company search))))
        (channels (sort (copy-list (company-channels company))
                        '|STRING<| :key 'channel-name)))
    (with-pagination (channels channels :next-link next-link :prev-link prev-link)
      (dashboard-template :user user :company company :script-name "/channels" :title "Screenshotbot: Channels"
       <taskie-page-title title= (channel-page-title) >

         <div class= "input-group mb-3">
           <span class= "input-group-text channel-search border-0" >
             <mdi name= "search" />
           </span>
           <input class= "form-control search d-inline-block border-0" type= "text" autocomplete= "off"
                  placeholder= "Search channels"
                  data-target= "#channel-result" />
          </div>
       </taskie-page-title>

       <div id= "channel-result" data-args= "{}" data-update=channel-search
            data-save-original= "true" >
         ,(%render-channels-as-taskie channels :next-link next-link
                                      :prev-link prev-link)
       </div>))))


(defhandler (projects-page :uri "/channels") ()
  (with-login ()
    (%list-projects)))

(defhandler (channels-page :uri "/projects") ()
  (hex:safe-redirect 'projects-page))

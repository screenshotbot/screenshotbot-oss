;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/web-build/project
  (:nicknames :screenshotbot/pro/web-build
              :screenshotbot/pro/web-build/project)
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class
                #:with-transaction
                #:store-object-id
                #:store-object-with-id)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:screenshotbot/server
                #:with-login
                #:staging-p
                #:defhandler)
  (:import-from #:screenshotbot/model/core
                #:ensure-slot-boundp
                #:has-created-at)
  (:import-from #:screenshotbot/taskie
                #:timeago
                #:taskie-timestamp
                #:taskie-row
                #:taskie-page-title
                #:taskie-list)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:screenshotbot/user-api
                #:can-view!
                #:current-user
                #:created-at
                #:%created-at
                #:current-company)
  (:import-from #:screenshotbot/ui/core
                #:ui/div
                #:ui/a)
  (:import-from #:screenshotbot/ui/confirmation-page
                #:confirmation-modal
                #:confirmation-page)
  (:import-from #:hunchentoot-extensions
                #:get-request-domain-prefix)
  (:import-from #:screenshotbot/model/api-key
                #:api-key-secret
                #:make-transient-key)
  (:import-from #:screenshotbot/api-key-api
                #:api-key-secret-key
                #:api-key-key)
  (:import-from #:screenshotbot/replay/remote
                #:run-thread-id
                #:remote-runs-for-company
                #:remote-run
                #:log-file
                #:remote-url
                #:remote-run-status
                #:donep
                #:remote-oid
                #:send-remote-run)
  (:import-from #:screenshotbot/model/user
                #:user-with-email)
  (:import-from #:util/object-id
                #:find-by-oid)
  (:import-from #:util/threading
                #:safe-interrupt)
  (:import-from #:screenshotbot/dashboard/explain
                #:explain)
  (:import-from #:scheduled-jobs
                #:make-scheduled-job
                #:make-scheduled-job)
  (:import-from #:scheduled-jobs/model
                #:cronexpr)
  (:import-from #:scheduled-jobs/bindings
                #:invalid-cron-expr-message
                #:invalid-cron-expr
                #:cron-parse-expr)
  (:import-from #:screenshotbot/model/company
                #:company-owner
                #:company-admins)
  (:import-from #:screenshotbot/installation
                #:installation
                #:installation-domain)
  (:local-nicknames (#:a #:alexandria)
                    (#:integration #:screenshotbot/replay/integration)
                    (#:frontend #:screenshotbot/replay/frontend)
                    (#:data #:bknr.datastore))
  (:export
   #:browser
   #:get-browsers
   #:next-job-at
   #:update-next-job-at))
(in-package :screenshotbot/web-build/project)

(markup:enable-reader)

(declaim (ftype (function (project) t) update-next-job-at))

(defclass web-project (store-object)
  ((name :initarg :name
         :accessor web-project-name)
   (urls :initarg :urls
         :initform nil
         :accessor urls)
   (sitemap :initarg :sitemap
            :initform nil
            :accessor sitemap)
   (browsers :initarg :browsers
             :initform nil
             :accessor browsers)
   (company :initarg :company
            :accessor company
            :index-type hash-index
            :index-reader web-projects-for-company)
   (remote-runs :initform nil
                :accessor remote-runs)
   (scheduled-job :initform nil
                  :initarg :scheduled-job
                  :accessor web-project-scheduled-job
                  :relaxed-object-reference t)
   (schedule-p :initform nil
               :initarg :schedule-p
               :accessor web-project-schedule-p)
   (schedule-every :initform nil
                   :initarg :schedule-every
                   :accessor web-project-schedule-every)
   (next-job-at :initform nil
                :accessor next-job-at)
   (custom-css :initform nil
               :initarg :custom-css
               :accessor custom-css)
   (created-at :initarg :created-at
               :initform 0
               :reader %created-at))
  (:metaclass persistent-class)
  (:default-initargs :created-at (get-universal-time)))

(defmethod push-remote-run (build remote-run)
  (with-transaction ()
    (push remote-run (remote-runs build))))

(defclass browser (store-object)
  ((name :initarg :name
         :accessor browser-name)
   (width :initarg :width
          :accessor width
          :initform nil)
   (height :initarg :height
           :accessor height
           :initform nil)
   (type :initarg :type
         :initform "chrome"
         :accessor browser-type)
   (company :initarg :company
            :index-type hash-index
            :index-reader %browsers-for-company
            :reader company)
   (mobile-emulation :initarg :mobile-emulation
                     :accessor mobile-emulation
                     :initform nil))
  (:metaclass persistent-class))

(defun get-browsers ()
  (let ((company (current-company)))
    (let ((browsers (%browsers-for-company company)))
      (or
       browsers
       (list
        (make-instance 'browser
                        :name "Google Chrome Desktop"
                        :width 1280
                        :company company
                        :height 800
                        :type "chrome")
        (make-instance 'browser
                        :name "Nexus 5 (emulated)"
                        :company company
                        :mobile-emulation "Nexus 5"
                        :type "chrome")
        (make-instance 'browser
                        :name "Firefox Desktop"
                        :width 1280
                        :company company
                        :height 800
                        :type "firefox"))))))

(defun summary (build)
  (format nil "~a on ~a"
          (cond
            ((sitemap build)
             "Sitemap")
            (t
             (format nil "~a urls " (length (urls build)))))
          (cond
            ((= (length (browsers build)) 1)
             (browser-name (car (browsers build))))
            (t
             (format nil "~a browsers"
                     (length (browsers build)))))))

(defhandler (nil :uri "/web-projects") ()
  (app-template :title "Screenshotbot: Web Projects"
   <div>
     ,(taskie-page-title
       :title "Web projects"
       <a href= (nibble () (new-project)) class= "btn btn-primary">
         New Project
       </a>
       <a href= "/browsers" class= "btn btn-secondary ms-1">
         Browser Configurations
       </a>

       <a class= "ms-1" href= (nibble () (recent-runs))>All runs</a>)
     ,(taskie-list
       :items (web-projects-for-company (current-company))
       :empty-message "No web projects"
       :row-generator (lambda (build)
                        (taskie-row
                         :object build
                         <span>
                           <a href=(nibble () (view-build build)) >
                             ,(web-project-name build)
                           </a>
                         </span>

                         (ui/div
                          <span>,(summary build) </span>)
                         (cond
                           ((and
                             (web-project-schedule-p build)
                             (web-project-scheduled-job build))
                            (taskie-timestamp
                             :prefix "Next run"
                             :timestamp (scheduled-jobs:at (web-project-scheduled-job build))))

                           ((and
                             (web-project-schedule-p build)
                             (next-job-at build))
                            (taskie-timestamp
                             :prefix "Next run"
                             :timestamp (next-job-at build)))
                           (t
                            <ul class= "list-inline font-13 text-end" >
                              <li class= "list-inline-item" >
                                <em>Not scheduled</em>
                              </li>
                            </ul>)))))
   </div>))

(defun recent-runs ()
  <app-template title="Recent Web runs">
    <render-runs runs= (remote-runs-for-company (current-company)) />
  </app-template>)

(defun view-build (build)
  <app-template title= (format nil "Screenshotbot: ~a" (web-project-name build)) >
    <div class= "page-title-box">
      <h3 class= "page-title" >
        ,(web-project-name build)
        <a data-href= (nibble () (run-now build)) class="btn btn-primary modal-link">Run Now</a>
        <a href= (nibble () (edit-build build)) class= "btn btn-secondary" >Edit Project</a>
      </h3>
    </div>
    <render-runs runs=(remote-runs build) />
  </app-template>)


(markup:deftag render-runs (&key runs)
      <table class= "table" >
      <thead>
        <tr>
          <th>Started at</th>
          <th>Done</th>
        </tr>
      </thead>
      <tbody>
        ,@ (loop for run in (util/lists:head runs 40)
                 for cancel = (util:copying (run)
                                (nibble ()
                                  (let ((back "/web-projects"))
                                    (confirmation-modal
                                     :yes
                                     (nibble ()
                                       #+lispworks
                                       (multiple-value-bind (thread-id thread)
                                           (run-thread-id run)
                                         (declare (ignore thread-id))
                                         (when thread
                                           (safe-interrupt thread)))
                                       (hex:safe-redirect back))
                                     <span>Interrupt job?</span>))))
                 collect
                 (util:copying (run cancel)
                   <tr>
                     <td>
                       <a href= (nibble () (remote-run-logs run)) >
                         ,(timeago :timestamp (created-at run))
                       </a>
                     </td>
                     <td title= (run-thread-id run) >
                       ,(cond
                          ((donep run)
                           (case (remote-run-status run)
                             (:success
                              "Finished")
                             (:user-aborted
                              "Aborted by user")
                             (otherwise
                              "Unknown error")))
                          (t
                           (case (remote-run-status run)
                             (:unknown
                              "Unknown state")
                             (:queued
                              "Queued")
                             (otherwise
                              <span>Running <a href= "#" class= "modal-link" data-href=cancel >Cancel</a> </span>))))
                     </td>
                   </tr>))
      </tbody>
    </table>
    )

(defun https? ()
  (string-equal "https" (hunchentoot:header-in* :x-forwarded-proto)))

(defun remote-run-log-endpoint (remote-run)
  (cond
    ((log-file remote-run)
     (format nil "~a://~a/wsapp/replay/logs/~a"
             (if (https?)
                 "wss"
                 "ws")
             (hunchentoot:host)
             (util:oid remote-run)))
    (t
     (format nil "~a/wsapp/replay/logs/~a"
             (str:replace-all "https:" "wss:" (remote-url remote-run))
             (remote-oid remote-run)))))

(defun remote-run-logs (remote-run)
  <app-template>
    <textarea data-websocket-stream= (remote-run-log-endpoint remote-run)
              disabled= "disabled"
              style="height:100vh; width: 100%" />
  </app-template>)

(defhandler (nil :uri "/replay/logs/:oid") (oid)
  (with-login ()
   (let ((remote-run (find-by-oid oid)))
     (format nil "Got ~s" remote-run)
     (check-type remote-run remote-run)
     (can-view! remote-run)
     (remote-run-logs remote-run))))

(defun fix-cronexpr (name cronexpr)
  (let ((hash (md5:md5sum-string name)))
    (format nil "~a ~a ~a"
            ;; not truly random, but good enough
            (mod (elt hash 0) 60)
            (mod (elt hash 1) 60)
            cronexpr)))

(defun unfix-cronexpr (cronexpr)
  (third (str:split " " cronexpr :limit 3)))

(defun form (&key submit browsers
               name
               urls
               custom-css
               sitemap
               schedule-p
               cronexpr)
  (flet ((option (value title)
           <option value=value
                   selected= (if (member value (mapcar #'store-object-id browsers)) markup:+empty+)
                   data-b=(mapcar #'store-object-id browsers) >
             ,(progn title)
           </option>))
   <simple-card-page max-width= "50em" form-action=submit >
     <div class= "card-header">
       <h3>
         Update Web Project configuration
       </h3>
     </div>
      <div class= "form-group mb-3">
        <label for= "name" class= "form-label">Project Name</label>
        <input type= "text" id= "name" name= "name" class= "form-control"
               value=name />
      </div>

      <div class= "form-group mb-3">
        <label for= "urls" class= "form-label">URLs (separate with newline)</label>
        <textarea id= "urls" name= "urls" class= "form-control" >,(progn urls)</textarea>
      </div>

      <div class= "form-group mb-3">
        <label for= "sitemap" class= "form-label">Link to sitemap (optional)</label>
        <input type= "text" id= "sitemap" name= "sitemap" class= "form-control"
               value=sitemap />
      </div>

      <div class= "form-group mb-3">
        <label for= "browsers" class= "form-label">
          Pick upto 5 browsers to run on
          ,(when (staging-p)
             <span>
               (<a href= "/browsers">Add or modify Browsers</a>)
             </span>)
        </label>
        <select id= "browsers" name= "browsers" class= "form-select" multiple >
          ,@(loop for browser in (get-browsers)
                  collect (option (store-object-id browser)
                                  (format nil "~a" (browser-name browser))))
        </select>
      </div>

      <div class= "form-group mb-3">
        <label for= "custom-css" class= "form-label">Override CSS (Advanced) </label>
        <textarea id= "custom-css" name= "custom-css" class= "form-control" >,(progn custom-css)</textarea>
      </div>

      <div class= "form-group mb-3">
        <input type= "checkbox" id= "schedule-p" name= "schedule-p" class= "form-check-input"
               checked= (when schedule-p "checked") />
        <label for= "schedule-p" class= "form-check-label" >Schedule <explain-cron-expr />
        </label>

        <input type= "input" name= "cronexpr" id= "cronexpr"
               class= "form-control mt-1"
               placeholder= "*/3 * * *"
               value= cronexpr />

      </div>


      <div class= "card-footer">
        <input type= "submit" class= "btn btn-primary" value= "Save" />
        <a href= "/web-projects" class= "btn btn-secondary">Cancel</a>
      </div>
  </simple-card-page>))

(defun run-now (build)
  "The name of this function is important, it's stored in the scheduled jobs on disk"
  (restart-case
      (confirmation-modal
       :title (web-project-name build)
       :yes (nibble ()  (actually-run-now build)
              (hex:safe-redirect (nibble ()
                                   (view-build build))))
       <p>Schedule a run for ,(web-project-name build)?</p>)
    (retry-run-now ()
      (run-now build))))

(defun scheduled-job-run-now (project)
  (cond
    ((and scheduled-jobs:*scheduled-job*
          (not
           (eql scheduled-jobs:*scheduled-job* (web-project-scheduled-job project))))
     (warn "Stale scheduled job for project: ~a" scheduled-jobs:*scheduled-job*))
    (t
     (let ((company (company project)))
       (actually-run-now
        project
        :user (or
               (company-owner company)
               (car (company-admins company)))
        :company company
        :host (installation-domain (installation)))))))

(defun actually-run-now (build
                         &key (user (current-user))
                           (company (current-company))
                           (host (get-request-domain-prefix)))
  (assert user)
  (assert company)
  (restart-case
      (let* ((run (make-instance 'integration:run
                                 :user user
                                 :company company
                                 :sitemap (unless (str:emptyp (sitemap build))
                                            (sitemap build))
                                 :urls (loop for url in (urls build)
                                             collect (cons url url))
                                 :custom-css (custom-css build)
                                 :channel (web-project-name build)
                                 :host host
                                 :browser-configs
                                 (loop for browser in (browsers build)
                                       collect
                                       (make-instance 'frontend:browser-config
                                                      :type (browser-type browser)
                                                      :mobile-emulation (mobile-emulation browser)
                                                      :dimensions
                                                      (when (width browser)
                                                        (make-instance 'frontend:dimensions
                                                                       :width (width browser)
                                                                       :height (height browser)))
                                                      :name (browser-name browser))))))
        (check-type (urls build) list)
        (let ((remote-run
                (send-remote-run run :company company)))
          (push-remote-run build remote-run)
          "OK"))
    (retry-actually-run-now ()
      (actually-run-now build))))

(defun new-project ()
  (let (submit)
    (setf submit (nibble (name
                          urls
                          sitemap
                          schedule-p
                          cronexpr)
                   (form-submit :name name
                                :urls urls
                                :sitemap sitemap
                                :schedule-p schedule-p
                                :cronexpr cronexpr
                                :submit submit)))
    (form :submit submit)))

(defun multiple-parameters (name)
  (loop for (key . value) in (hunchentoot:post-parameters*)
        if (string-equal name key)
          collect value))

(defun fix-cron-message (message)
  "Fixes the cron message from cron-parse-expr to be user-friendly"
  (cond
    ((str:containsp "Invalid number" message)
     (str:replace-all "6 fields" "4 fields" message))
    (t (format nil "Error from cron parser: ~A" message))))

(markup:deftag explain-cron-expr ()
  <explain>
    <p>
      Use a cron like syntax to schedule your jobs. e.g. to schedule every every day at 8am, do
      <span class= "text-monospace" >8 * * *</span>
    </p>

    <p>
      Unlike Cron you can't specify minutes, Screenshotbot will pick a random minute.
    </p>

    <p>
      The fields are respectively: Hour, Day of Month, Month, Day of Week.
    </p>

    <h6>Examples</h6>

    <p>
      To schedule every three hours: */3 * * *. To schedule 8am and 4pm on weekdays do: 8,4 * * MON-FRI.
    </p>
  </explain>)

(defun form-submit (&rest args &key name urls sitemap submit
                                 edit
                                 schedule-p
                                 custom-css
                                 cronexpr
                                 (browsers (multiple-parameters "browsers")))
  (restart-case
      (let ((errors))
        (flet ((check (test field message)
                 (unless test
                   (push (cons field message) errors))))
          (check (not (str:emptyp name)) :name "Please provide a project name")
          (let ((urls (remove-if #'str:emptyp (mapcar #'str:trim (str:lines urls))))
                (sitemap (str:trim sitemap))
                (browsers (loop for browser in browsers
                                for obj = (store-object-with-id (parse-integer browser))
                                if (eql (current-company)
                                        (company obj))
                                  collect obj)))
            (cond
              ((not (str:emptyp sitemap))
               (check (ignore-errors (quri:parse-uri sitemap))
                      :sitemap "Invalid URL"))
              (t
               (check urls :urls "Must provide at least one URL")))
            (check browsers
                   :browsers
                   "Please select at least one browser")
            (check (or
                    (str:emptyp sitemap)
                    (null urls))
                   :sitemap
                   "Please provide only one of URLs or sitemap")
            (check (< (length browsers) 5)
                   :browsers
                   "Too many browsers selected")

            (when schedule-p
              (handler-case
                  (cron-parse-expr (fix-cronexpr name  cronexpr))
                (invalid-cron-expr (e)
                  (let ((message (invalid-cron-expr-message e)))
                    (check nil
                           :cronexpr
                           (fix-cron-message message))))))
            (cond
              (errors
               (with-form-errors (:errors errors
                                  :name name
                                  :cronexpr cronexpr
                                  :schedule-p :schedule-p
                                  :urls (str:join #\Newline urls)
                                  :sitemap sitemap
                                  :was-validated t)
                 (form :submit submit
                       :browsers browsers)))
              ((not edit)
               (let ((project
                      (make-instance 'web-project
                                      :name name
                                      :company (current-company)
                                      :schedule-p schedule-p
                                      :urls urls
                                      :custom-css custom-css
                                      :browsers browsers
                                      :sitemap sitemap)))
                 (update-scheduled-job project
                                       schedule-p
                                       cronexpr))

               (hex:safe-redirect "/web-projects"))
              (edit
               (with-transaction ()
                 (setf (web-project-name edit) name)
                 (setf (urls edit) urls)
                 (setf (browsers edit) browsers)
                 (Setf (web-project-schedule-p edit) schedule-p)
                 (setf (custom-css edit) custom-css)
                 (setf (sitemap edit) sitemap))
               (update-scheduled-job edit
                                     schedule-p
                                     cronexpr)
               (hex:safe-redirect "/web-projects"))))))
    (retry-form-submit ()
      (apply #'form-submit args))))

(defun update-scheduled-job (self schedule-p cronexpr)
  (a:when-let ((scheduled-job (web-project-scheduled-job self)))
    (with-transaction ()
     (setf (web-project-scheduled-job self) nil))
    (unless (bknr.datastore::object-destroyed-p scheduled-job)
      (bknr.datastore:delete-object scheduled-job)))
  (when schedule-p
    (let ((job (make-scheduled-job :cronexpr (fix-cronexpr (web-project-name self) cronexpr)
                                   :tzname "America/New_York" ;; TODO
                                   :function 'scheduled-job-run-now
                                   :args (list self))))
      (with-transaction ()
        (setf (web-project-scheduled-job self) job)))))

(defun edit-build (build)
  (let (submit)
    (setf submit
          (nibble (name urls sitemap
                        schedule-p
                        custom-css
                        cronexpr)
            (form-submit :name name
                         :urls urls
                         :sitemap sitemap
                         :edit build
                         :schedule-p schedule-p
                         :custom-css custom-css
                         :cronexpr cronexpr
                         :submit submit)))
   (form :name (web-project-name build)
         :urls (str:join #\Newline (urls build))
         :sitemap (sitemap build)
         :browsers (browsers build)
         :custom-css (custom-css build)
         :submit submit
         :schedule-p (web-project-schedule-p build)
         :cronexpr (a:when-let (scheduled-job
                                (web-project-scheduled-job build))
                     (unless (bknr.datastore::object-destroyed-p scheduled-job)
                      (unfix-cronexpr (cronexpr  scheduled-job)))))))

#+nil
(defun migrate-to-cronexpr ()
  (loop for build in (bknr.datastore:store-objects-with-class 'web-project)
        if (and
            (not (web-project-scheduled-job build))
            (web-project-schedule-p build)
            (web-project-schedule-every build))
          collect build))

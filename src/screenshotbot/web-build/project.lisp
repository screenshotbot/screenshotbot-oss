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
  (app-template :title "Screenshotbot: Web Projectslo"
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
                           ((web-project-schedule-p build)
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

(progn
  <option value=nil />)

(defun form (&key submit browsers
               name
               urls
               custom-css
               sitemap
               schedule-p
               schedule-every)
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
        <label for= "schedule-p" class= "form-check-label" >Run every
          <input type= "number" name= "schedule-every" id= "schedule-every"
                 max= "48" min= "0" style= "width: 3em"
                 value=schedule-every />
          hours.
        </label>
      </div>


      <div class= "card-footer">
        <input type= "submit" class= "btn btn-primary" value= "Save" />
        <a href= "/web-projects" class= "btn btn-secondary">Cancel</a>
      </div>
  </simple-card-page>))

(defun run-now (build)
  (restart-case
      (confirmation-modal
       :title (web-project-name build)
       :yes (nibble ()  (actually-run-now build)
              (hex:safe-redirect (nibble ()
                                   (view-build build))))
       <p>Schedule a run for ,(web-project-name build)?</p>)
    (retry-run-now ()
      (run-now build))))

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
                          schedule-every)
                   (form-submit :name name
                                :urls urls
                                :sitemap sitemap
                                :schedule-p schedule-p
                                :schedule-every schedule-every
                                :submit submit)))
    (form :submit submit)))

(defun multiple-parameters (name)
  (loop for (key . value) in (hunchentoot:post-parameters*)
        if (string-equal name key)
          collect value))

(defun form-submit (&rest args &key name urls sitemap submit
                                 edit
                                 schedule-p
                                 custom-css
                                 schedule-every
                                 (browsers (multiple-parameters "browsers")))
  (restart-case
      (let ((errors)
            (schedule-every (parse-integer (if (str:emptyp schedule-every) "0" schedule-every))))
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
             (check (< 2 schedule-every)
                    :schedule-p
                    (format nil "Schedule is too low ~S" schedule-p)))
            (cond
              (errors
               (with-form-errors (:errors errors
                                  :name name
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
                                      :urls urls
                                      :schedule-p (when schedule-p t)
                                      :schedule-every schedule-every
                                      :custom-css custom-css
                                      :browsers browsers
                                      :sitemap sitemap)))
                 (update-next-job-at project))

               (hex:safe-redirect "/web-projects"))
              (edit
               (with-transaction ()
                 (setf (web-project-name edit) name)
                 (setf (urls edit) urls)
                 (setf (browsers edit) browsers)
                 (setf (custom-css edit) custom-css)
                 (setf (sitemap edit) sitemap)
                 (setf (web-project-schedule-p edit)
                       (when schedule-p t))
                 (setf (web-project-schedule-every edit)
                       schedule-every))
               (update-next-job-at edit)
               (hex:safe-redirect "/web-projects"))))))
    (retry-form-submit ()
      (apply #'form-submit args))))

(defun edit-build (build)
  (let (submit)
    (setf submit
          (nibble (name urls sitemap
                        schedule-p
                        custom-css
                        schedule-every)
            (form-submit :name name
                         :urls urls
                         :sitemap sitemap
                         :edit build
                         :schedule-p schedule-p
                         :custom-css custom-css
                         :schedule-every schedule-every
                         :submit submit)))
   (form :name (web-project-name build)
         :urls (str:join #\Newline (urls build))
         :sitemap (sitemap build)
         :browsers (browsers build)
         :custom-css (custom-css build)
         :submit submit
         :schedule-p (web-project-schedule-p build)
         :schedule-every (web-project-schedule-every build))))

(defun run-chris ()
  (let ((build (data:store-object-with-id 667899)))
    (actually-run-now build
                      :user (user-with-email "chrispecoraro@gmail.com")
                      :company (company build)
                      :host "https://screenshotbot.io")))

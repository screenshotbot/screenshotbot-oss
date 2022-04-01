;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/run-page
  (:use #:cl
        #:alexandria
        #:markup
        #:nibble
        #:screenshotbot/promote-api
        #:screenshotbot/report-api
        #:screenshotbot/user-api
        #:screenshotbot/git-repo
        #:screenshotbot/screenshot-api
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/view
        #:screenshotbot/model/screenshot
        #:screenshotbot/template
        #:screenshotbot/model/image
        #:screenshotbot/model/channel
        #:screenshotbot/model/company)
  (:import-from #:screenshotbot/server
                #:make-thread
                #:defhandler)
  (:import-from #:util
                #:oid
                #:find-by-oid)
  (:import-from #:hex #:make-url)
  (:import-from #:screenshotbot/ui
                #:ui/a
                #:ui/div)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:screenshotbot/dashboard/paginated
                #:paginated)
  (:import-from #:screenshotbot/model/report
                #:reports-for-run)
  (:import-from #:screenshotbot/report-api
                #:report-title)
  (:import-from #:screenshotbot/model/image
                #:image-dimensions)
  (:export
   #:*create-issue-popup*
   #:run-page
   #:run-row-filter
   #:row-filter
   #:filter-selector
   #:commit)
  (:export #:mask-editor))
(in-package :screenshotbot/dashboard/run-page)

(markup:enable-reader)

(defvar *create-issue-popup* nil
  "On the non-OSS Screenshotbot, we have a feature to Jira tasks
  directly from Screenshotbot by annotating images. This is just a
  reference to that function. If there are requests for it, we'll open
  source it too.")

(deftag commit (&key repo hash)
  (ui/a :href (commit-link repo hash)
    (str:substring 0 8 hash)))

(defhandler (run-page :uri "/runs/:id" :method :get) (id name)
  (let* ((run (find-by-oid id 'recorder-run)))
    (render-run-page run :name name)))

(deftag page-nav-dropdown (children &key title)
  (let ()

    (mquery:add-class (mquery:$ "a" children) "dropdown-item")

    <div class="dropdown">
  <button class="btn btn-sm btn-secondary dropdown-toggle" type="button" id="dropdownMenuButton" data-bs-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
    ,(progn title)
  </button>
  <div class="dropdown-menu" aria-labelledby="dropdownMenuButton" style= "z-index: 99999999; position: static" >
    ,@(progn children)
  </div>
    </div>))

(defun promotion-log-page (run)
  <app-template>
  <section>
  <pre>
  --BEGIN-LOGS--
,(let ((file (bknr.datastore:blob-pathname (promotion-log run))))
      (cond
        ((path:-e file)
         (uiop:read-file-string file))
        (t
         "Log file unavailable")))
  --END-LOGS--
  </pre>
  </section>
  </app-template>)

(defun run-size (run)
  "Get the total size of all the screenshots associated with this run"
  (floor
   (loop for screenshot in (recorder-run-screenshots run)
         summing
         (with-local-image (file screenshot)
           (trivial-file-size:file-size-in-octets file)))
   1024))

(defun screenshots-above-16k-dim (run)
  "Get the number of screenshots that have a dimension above 16k. This
  is the limit imposed by webp images."
  (let ((+limit+ 16383))
    (loop for screenshot in (recorder-run-screenshots run)
          for dim = (image-dimensions screenshot)
          if (or
              (> (dimension-height dim) +limit+)
              (> (dimension-width dim) +limit+))
            collect screenshot)))

(deftag advanced-run-page (&key run)
  (let ((repo (channel-repo (recorder-run-channel run))))
    <app-template>
      <h2>Debug Run Information</h2>
      <ul>
        <li>Repo url: ,(github-repo run)</li>
        <li>Commit: ,(commit :repo repo :hash (recorder-run-commit run)) </li>
        <li>Main Branch: ,(recorder-run-branch run)</li>
        <li>Commit on branch: ,(commit :repo repo :hash (recorder-run-branch-hash run))</li>
        <li>Merge base: ,(commit :repo repo :hash (recorder-run-merge-base run))</li>
        <li>Pull request: ,(or (pull-request-url run) "NA")</li>
        <li>Phabricator Diff-id: ,(or (phabricator-diff-id run) "NA")</li>
        <li>Build URL: <a href=(run-build-url run)>,(run-build-url run)</a> </li>
        <li>Periodic job?: ,(if (periodic-job-p run) "true" "false")</li>
        <li>Number of screenshots: ,(length (recorder-run-screenshots run))</li>
        <li>Total run size: ,(run-size run)kB</li>

        <li>Screenshots that are above 16k dimensions: ,(length (screenshots-above-16k-dim run))

        <a href= (nibble ()
                           <app-template>
                             List of screenshots with large dimensions
                             <ul>
                               ,@(loop for x in (screenshots-above-16k-dim run)
                                       collect <li>
                                 <a href= (make-url 'run-page :id (oid run) :name (screenshot-name x))>
                                   ,(screenshot-name x)
                                               </a></li>)
                             </ul>
                           </app-template>)>
                           List
       </a>
        </li>
      </ul>
    </app-template>))

(deftag run-advanced-menu (&key run)
  (let ((promotion-logs (nibble ()
                          (promotion-log-page run)))
        (rerun-promotions (nibble ()
                            (make-thread
                             (lambda ()
                               (log:info "Starting re-run promotion thread")
                               (start-promotion-thread
                                (recorder-run-channel run)
                                run)))
                            (hex:safe-redirect 'run-page
                                                :id (oid run))))
        (debug-info (nibble ()
                      (advanced-run-page :run run))))
    <page-nav-dropdown title= "Advanced">
      <a href= promotion-logs >Promotion Logs</a>
      <a href= rerun-promotions >Re-Run Promotions</a>
      <a href=debug-info >Debug Info</a>
    </page-nav-dropdown>))

(defun create-filter-matcher (filter &key key)
  (cond
    ((eq t filter)
     'identity)
    (t (lambda (x) (equal (funcall key x)
                          filter)))))

(defclass row-filter ()
  ((key :initarg :key
        :initform 'identity
        :accessor row-filter-key)
   (value :initarg :value
          :accessor row-filter-value)))

(defun run-row-filter (row-filter list)
  (let ((list (if (listp list) list (list list))))
   (let ((filter (row-filter-value row-filter)))
     (loop for x in list
           if (or
               (eq filter t)
               (equal filter (funcall (row-filter-key row-filter) x)))
             collect x))))

(deftag filter-selector (&key default-title
                         prefix
                         data
                         filter-renderer
                         row-filter)
  (let ((devices (remove-duplicates
                  (loop for row in data
                        collect (funcall (row-filter-key row-filter) row))
                  :test 'equal))
        (filter (row-filter-value row-filter)))

    (cond
      ((<= (length devices) 1)
       <markup:merge-tag />)
      (t
       <page-nav-dropdown title= (if (eq filter t) default-title (format nil "~a: ~a"  prefix filter)) >
       <a href= (nibble () (funcall filter-renderer t)) >,(progn default-title) </a>
       ,@ (loop for device in devices
                collect
                (let ((device device))
                  (let ((device-selector (nibble ()
                                           (funcall filter-renderer
                                                    device))))
                    <a href=device-selector >,(progn device)</a>)) )
       </page-nav-dropdown>))))


(defun render-run-page (run &rest filters &key name)
  (can-view! run)
  (flet ((re-call (&rest args)
           (apply 'render-run-page run (append args filters))))

    (let* ((channel (recorder-run-channel run))
           (screenshots (recorder-run-screenshots run))
           (filtered-screenshots (cond
                                   (name
                                    (loop for s in screenshots
                                          if (string-equal (screenshot-name s) name)
                                            collect s))
                                   (t
                                    screenshots))))
      <app-template >
        <div class= "page-title-box">
          <h4 class= "page-title" >Run from
            <:time class= "timeago" datetime= (created-at run)>
              ,(created-at run)
            </:time>
          </h4>

          <div class= "d-flex justify-content-between mt-3 mb-3">
            <div class= "" style= "width: 20em" >
              <div class= "input-group">
                <span class= "input-group-text" >
                  <mdi name= "search" />
                </span>
                <input class= "form-control search d-inline-block" type= "text" autocomplete= "off"
                       placeholder= "Search..."
                       data-target= "#run-page-results" />
              </div>
            </div>


            <div class= "">
              <a class= "btn btn-danger btn-sm">Delete</a>
              <run-advanced-menu run=run />
            </div>
          </div>

        </div>

          ,(when-let (reports (reports-for-run run))
             <div class= "alert alert-info mt-2" >
               This run created a report with
               ,@ (loop for report in reports collect
                          <a href= (format nil "/report/~a" (oid report)) >,(report-title report)</a>)
             </div>)

          <div id= "run-page-results" class= "search-results" data-update= (nibble () (update-content run channel))
               data-args= "{}" >
            ,(run-page-contents run channel filtered-screenshots)
          </div>

      </app-template>)))

(defun update-content (run channel)
  (let* ((query (hunchentoot:parameter "search"))
         (screenshots (recorder-run-screenshots run)))
    (run-page-contents
     run channel (loop for screenshot in screenshots
                       if (or (null query)
                              (str:contains? query (screenshot-name screenshot)
                                             :ignore-case t))
                         collect screenshot))))

(defun run-page-contents (run channel filtered-screenshots)
  <div class= "baguetteBox" id= (format nil "a-~a" (random 10000000)) >

    ,(unless filtered-screenshots
       <p class= "text-muted" >No screenshots found</p>)

    ,(paginated
    (lambda (screenshot)
    (let* ((name-parts (str:rsplit "--" (screenshot-name screenshot) :limit 2)))
    <div class= " col-sm-12 col-md-4 col-lg-3 mb-1 mt-2">
      <div class="card">
        <div class="card-body">
          <div class= "screenshot-header" >
            <h4 >,(car name-parts)</h4>
            ,(when (cadr name-parts)
               <h6>,(cadr name-parts)</h6>)
            <ul class= "screenshot-options-menu">
              <li>
                <a href= (make-url 'history-page :channel (store-object-id channel)
                                                                                   :screenshot-name (screenshot-name screenshot))>
                  History
                </a>
              </li>

              <li>
                <a href= (nibble () (mask-editor (recorder-run-channel run) screenshot
                   :redirect (make-url 'run-page :id (oid run))))
                   >Edit Masks</a>

              </li>

              ,(when *create-issue-popup*
                 <li>
                   <a target= "_blank"
                      href= (nibble ()
                                      (funcall *create-issue-popup* run screenshot)) >
                     Create Issue
                   </a>
                 </li>)
            </ul>
          </div>
          <a href= (image-public-url (screenshot-image screenshot)) title= (screenshot-name screenshot) >
            <img class= "screenshot-image run-page-image" src= (image-public-url (screenshot-image screenshot)  :size :small) />
          </a>
        </div> <!-- end card-body-->
      </div>

    </div>))
    :items filtered-screenshots)
  </div>)

(defclass js-api-result () ())

(defclass js-api-success (js-api-result)
  ((success :type boolean
            :initform t)
   (was-promoted :type boolean
                 :initarg :was-promoted
                 :initform nil)))

(defhandler (run-delete-page :uri "/runs/:id" :method :delete :html nil) (id)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((run (find-by-oid id 'recorder-run)))
    (can-view! run)
    (when run
      (cond
        ((or (activep run)
             (recorder-previous-run run))
         (log:info "Can't delete: ~s this run seems to be a master run" run)
         (json:encode-json-to-string (make-instance 'js-api-success
                                                     :was-promoted t)))
        (t
         (bknr.datastore:with-transaction ()
           (setf (company-runs (current-company))
                 (remove run (company-runs (current-company)))))
         (json:encode-json-to-string (make-instance 'js-api-success)))))))

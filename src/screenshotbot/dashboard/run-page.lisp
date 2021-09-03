;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/run-page
    (:use #:cl
          #:alexandria
          #:markup
          #:nibble
          #:../promote-api
          #:../report-api
          #:../user-api
          #:../git-repo
          #:../screenshot-api
          #:../model/recorder-run
          #:../model/view
          #:../model/screenshot
          #:../template
          #:../model/image
          #:../model/channel
          #:../model/company)
  (:import-from #:../server
                #:make-thread
                #:defhandler)
  (:import-from #:util
                #:oid
                #:find-by-oid)
  (:import-from #:hex
                #:make-url)
  (:import-from #:../ui
                #:ui/a
                #:ui/div)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:export #:*create-issue-popup*
           #:run-page
           #:run-row-filter
           #:row-filter
           #:filter-selector
           #:commit)
  ;;forward decls
  (:export #:mask-editor))

(markup:enable-reader)

(defvar *create-issue-popup* nil
  "On the non-OSS Screenshotbot, we have a feature to Jira tasks
  directly from Screenshotbot by annotating images. This is just a
  reference to that function. If there are requests for it, we'll open
  source it too.")

(deftag commit (&key repo hash)
  (ui/a :href (commit-link repo hash)
    (str:substring 0 8 hash)))

(defhandler (run-page :uri "/runs/:id" :method :get) (id)
  (let* ((run (find-by-oid id 'recorder-run)))
    (render-run-page run :lang-filter t
                     :device-filter t)))

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

(deftag advanced-run-page (&key run)
  (let ((repo (channel-repo (recorder-run-channel run))))
    <app-template>
      <h2>Debug Run Information</h2>
      <ul>
        <li>Commit: ,(commit :repo repo :hash (recorder-run-commit run)) </li>
        <li>Main Branch: ,(recorder-run-branch run)</li>
        <li>Commit on branch: ,(commit :repo repo :hash (recorder-run-branch-hash run))</li>
        <li>Merge base: ,(commit :repo repo :hash (recorder-run-merge-base run))</li>
        <li>Pull request: ,(or (pull-request-url run) "NA")</li>
        <li>Phabricator Diff-id: ,(or (phabricator-diff-id run) "NA")</li>
        <li>Build URL: <a href=(run-build-url run)>,(run-build-url run)</a> </li>
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
                                run))
                             (hex:safe-redirect 'run-page
                                                  :id (oid run)))))
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


(defun render-run-page (run &rest filters &key lang-filter
                                            device-filter)
  (can-view! run)
  (flet ((re-call (&rest args)
           (apply 'render-run-page run (append args filters))))

    (let* ((channel (recorder-run-channel run))
           (lang-filter (make-instance 'row-filter :key 'screenshot-lang
                                                   :value lang-filter))
           (device-filter (make-instance 'row-filter :key 'screenshot-device
                                                     :value device-filter))
           (filtered-screenshots (run-row-filter device-filter
                                                 (run-row-filter lang-filter (recorder-run-screenshots run)))))
      <app-template >
        <div class= "page-title-box">
          <h4 class= "page-title" >Run from
            <:time class= "timeago" datetime= (created-at run)>
              ,(created-at run)
            </:time>
          </h4>
          <a class= "btn btn-danger btn-sm">Delete</a>
          <filter-selector default-title= "All Languages"
                           prefix= "Language"
                           row-filter=lang-filter
                           filter-renderer= (lambda (x) (re-call :lang-filter x))
                           data= (recorder-run-screenshots run)
                           />
          <filter-selector default-title="All Devices"
                           prefix= "Device"
                           row-filter=device-filter
                           filter-renderer= (lambda (x) (re-call :device-filter x))
                           data= (recorder-run-screenshots run)
                           />

      <run-advanced-menu run=run />
        </div>
        <div class= "row baguetteBox">
          ,@ (loop for screenshot in filtered-screenshots
          collect
          (let ((screenshot screenshot))
          <div class= " col-sm-12 col-md-4 col-lg-3">
            <div class="card">
              <div class="card-body">
                <div class= "screenshot-header" >
                  <h4 >,(screenshot-name screenshot)</h4>
                  <ul class= "screenshot-options-menu">
                    <li>
                    </li>
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
                  <img class= "screenshot-image run-page-image" src= (image-public-url (screenshot-image screenshot)) />
                </a>
              </div> <!-- end card-body-->
            </div>

          </div>))
        </div>
      </app-template>)))

(defclass js-api-result () ())

(defclass js-api-success (js-api-result)
  ((success :type boolean
            :initform t)))

(defhandler (run-delete-page :uri "/runs/:id" :method :delete :html nil) (id)
  (let ((run (find-by-oid id 'recorder-run)))
    (can-view! run)
    (when run
      (cond
        ((or (activep run)
             (recorder-previous-run run))
           (log:info "Can't delete: ~s this run seems to be a master run" run))
        (t (delete run (company-runs (current-company))))))
    (json:encode-json-to-string (make-instance 'js-api-success))))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/dashboard
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
  (:import-from #:screenshotbot/model/channel
                #:channel-name
                #:channel-company)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-channel
                #:recorder-run-commit
                #:recorder-run-screenshots
                #:runs-for-company
                #:activep)
  (:import-from #:screenshotbot/user-api
                #:current-company
                #:company-channels
                #:company-runs
                #:user-full-name
                #:user-image-url
                #:pull-request-url
                #:can-view!)
  (:import-from #:util/object-id
                #:oid)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-link)
  (:import-from #:core/ui/mdi
                #:mdi)
  (:import-from #:util/misc
                #:make-mp-hash-table)
  (:import-from #:util/timeago
                #:timeago)
  (:import-from #:screenshotbot/model/report
                #:report-title
                #:report-channel
                #:report-run
                #:acceptable-report
                #:acceptable-state
                #:acceptable-reviewer
                #:acceptable-history
                #:acceptable-history-item-state
                #:acceptable-history-item-user
                #:acceptable-history-item-ts
                #:base-acceptable
                #:acceptable)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:export #:dashboard-page))
(in-package :screenshotbot/dashboard/dashboard)

(named-readtables:in-readtable markup:syntax)

(defun get-recent-activity (company &optional (limit 10))
  "Get recent Accept/Reject activity from base-acceptable objects for this company"
  (let ((company-channels (company-channels company))
        (activity-items '()))
    ;; Get all acceptables for company channels and extract history items
    (loop for acceptable in (class-instances 'base-acceptable)
          for report = (ignore-errors (acceptable-report acceptable))
          for channel = (ignore-errors (report-channel report))
          when (and channel (member channel company-channels))
            do (handler-case
                   (progn
                     ;; Check authorization for all objects before adding to activity
                     (can-view! acceptable)
                     (can-view! report)
                     (can-view! channel)
                     (loop for history-item in (acceptable-history acceptable)
                           for state = (acceptable-history-item-state history-item)
                           for user = (acceptable-history-item-user history-item) 
                           for timestamp = (acceptable-history-item-ts history-item)
                           when (member state '(:accepted :rejected))
                             do (push (list :acceptable acceptable
                                            :action state
                                            :user user
                                            :timestamp timestamp
                                            :report report
                                            :channel channel)
                                      activity-items)))
                 (error ())))
    ;; Sort by timestamp (newest first) and limit
    (subseq (sort activity-items (lambda (a b) (> (getf a :timestamp) (getf b :timestamp))))
            0 (min limit (length activity-items)))))

(defun calculate-dashboard-metrics (company)
  "Calculate key metrics for the dashboard overview cards"
  (can-view! company)
  (let* ((all-runs (company-runs company))
         (all-channels (company-channels company))
         ;; Filter runs and channels to only those the user can view
         (runs (remove-if-not (lambda (run)
                                (handler-case
                                    (progn (can-view! run) t)
                                  (error () nil)))
                              all-runs))
         (channels (remove-if-not (lambda (channel)
                                    (handler-case
                                        (progn (can-view! channel) t)
                                      (error () nil)))
                                  all-channels)))
    (let ((total-runs (length runs))
          (active-runs (count-if #'activep runs))
          (recent-activity (get-recent-activity company))
          (failed-runs
            10
            #+nil
            (count-if (lambda (run)
                                   (and (< (length (recorder-run-screenshots run)) 1)
                                        (< (- (get-universal-time) (created-at run)) (* 24 3600))))
                                 runs)))
      (list :total-runs total-runs
            :active-runs active-runs
            :failed-runs failed-runs
            :success-rate (if (> total-runs 0)
                             (float (* 100 (/ (- total-runs failed-runs) total-runs)))
                             100.0)
            :recent-activity recent-activity
            :channels channels))))

(deftag dashboard-overview-cards (&key metrics)
  <div class="row mb-4">
    <div class="col-md-3">
      <div class="card text-center">
        <div class="card-body">
          <mdi name="camera" class="text-primary mb-2" style="font-size: 2rem;" />
          <h4 class="card-title">,(getf metrics :total-runs)</h4>
          <p class="card-text text-muted">Total Runs</p>
        </div>
      </div>
    </div>
    <div class="col-md-3">
      <div class="card text-center">
        <div class="card-body">
          <mdi name="flash_on" class="text-success mb-2" style="font-size: 2rem;" />
          <h4 class="card-title">,(getf metrics :active-runs)</h4>
          <p class="card-text text-muted">Active Tests</p>
        </div>
      </div>
    </div>
    <div class="col-md-3">
      <div class="card text-center">
        <div class="card-body">
          <mdi name="error" class="text-danger mb-2" style="font-size: 2rem;" />
          <h4 class="card-title">,(getf metrics :failed-runs)</h4>
          <p class="card-text text-muted">Failed Tests</p>
        </div>
      </div>
    </div>
    <div class="col-md-3">
      <div class="card text-center">
        <div class="card-body">
          <mdi name="trending_up" class="text-info mb-2" style="font-size: 2rem;" />
          <h4 class="card-title">,(format nil "~,1f%" (getf metrics :success-rate))</h4>
          <p class="card-text text-muted">Success Rate</p>
        </div>
      </div>
    </div>
  </div>)

(defun get-status-icon (run)
  "Get appropriate status icon for a run"
  (cond
    ((activep run) "check_circle")
    ((< (length (recorder-run-screenshots run)) 1) "error")
    (t "schedule")))

(defun get-status-class (run)
  "Get appropriate CSS class for run status"
  (cond
    ((activep run) "text-success")
    ((< (length (recorder-run-screenshots run)) 1) "text-danger")
    (t "text-warning")))

(defun get-activity-icon (action)
  "Get appropriate icon for accept/reject action"
  (case action
    (:accepted "check_circle")
    (:rejected "cancel")
    (t "help")))

(defun get-activity-class (action)
  "Get appropriate CSS class for accept/reject action"
  (case action
    (:accepted "text-success")
    (:rejected "text-danger")
    (t "text-muted")))

(defun get-activity-text (action)
  "Get human-readable text for action"
  (case action
    (:accepted "accepted")
    (:rejected "rejected")
    (t "unknown")))

(deftag activity-feed (&key recent-activity)
  <div class="card mb-4 recent-activity-feed">
    <div class="card-header d-flex justify-content-between align-items-center">
      <h5 class="mb-0">
        <mdi name="timeline" class="text-primary me-2" />
        Activity Feed
      </h5>
      <a href="/runs" class="btn btn-outline-primary btn-sm">View All Runs</a>
    </div>
    <div class="card-body p-0">
      ,(if recent-activity
         <div class="list-group list-group-flush">
           ,@(loop for activity-item in recent-activity
                   for action = (getf activity-item :action)
                   for user = (getf activity-item :user)
                   for timestamp = (getf activity-item :timestamp)
                   for channel = (getf activity-item :channel)
                   for report = (getf activity-item :report)
                   collect
                   (let ((activity-class (case action
                                         (:accepted "activity-accepted")
                                         (:rejected "activity-rejected")
                                         (t ""))))
                     <div class=(format nil "list-group-item d-flex justify-content-between align-items-center py-3 ~a" activity-class)>
                     <div class="d-flex align-items-center">
                       ,(if user
                          <div class="position-relative me-3 activity-avatar">
                            <img class="rounded-circle" src=(user-image-url user) width="32" height="32" alt="Profile" />
                            <span class=(format nil "activity-badge ~a" (get-activity-class action))>
                              <mdi name=(get-activity-icon action) />
                            </span>
                          </div>
                          <mdi name=(get-activity-icon action) class=(format nil "me-3 ~a" (get-activity-class action)) />)
                       <div>
                         <h6 class="mb-1">
                           ,(if user
                              (user-full-name user)
                              "Unknown user")
                           <span class="fw-normal">,(get-activity-text action)</span>
                           <span class="fw-normal">screenshots for</span>
                           <strong>,(channel-name channel)</strong>
                         </h6>
                         <small class="text-muted">
                           ,(let ((run (report-run report)))
                              (if (and run (pull-request-url run))
                                  <a href=(pull-request-url run) target="_blank" class="text-decoration-none">PR Review</a>
                                  <span>PR Review</span>))
                           ,(when (report-run report)
                              <span>• ,(recorder-run-commit (report-run report))</span>)
                         </small>
                       </div>
                     </div>
                     <div class="d-flex align-items-center">
                       <small class="text-muted me-3"><timeago timestamp=timestamp /></small>
                       <a href=(format nil "/report/~a" (oid report)) class="btn btn-outline-primary btn-sm">View</a>
                     </div>
                   </div>))
         </div>
         <div class="text-center py-4 text-muted">
           <mdi name="timeline" style="font-size: 3rem;" class="mb-2" />
           <p>No recent activity</p>
           <small>Accept or reject screenshot changes to see activity here</small>
         </div>)
    </div>
  </div>)

(defun calculate-channel-health (channel)
  "Calculate health percentage for a channel based on recent runs"
  100)

(defun get-health-color (percentage)
  "Get Bootstrap color class based on health percentage"
  (cond
    ((>= percentage 80) "success")
    ((>= percentage 60) "warning")
    (t "danger")))

(deftag progress-bar (&key percentage)
  (let ((color (get-health-color percentage)))
    <div class="progress" style="height: 8px;">
      <div class="progress-bar" role="progressbar" 
           style=(format nil "width: ~a%; background-color: var(--bs-~a);" percentage color)
           aria-valuenow=percentage aria-valuemin="0" aria-valuemax="100">
      </div>
    </div>))

(deftag channel-status-grid (&key channels)
  <div class="row channel-status-grid">
    <div class="col-md-8">
      <div class="card channel-grid-card">
        <div class="card-header">
          <h5 class="mb-0">
            <mdi name="list" class="me-2" />
            Active Channels
          </h5>
        </div>
        <div class="card-body">
          ,(if channels
             <div>
               ,@(loop for channel in (subseq channels 0 (min 6 (length channels)))
                       for health = (calculate-channel-health channel)
                       collect
                       <div class="d-flex justify-content-between align-items-center mb-3">
                         <div class="d-flex align-items-center">
                           <mdi name="book" class="me-3 text-primary" />
                           <div>
                             <h6 class="mb-1">,(channel-name channel)</h6>
                             <progress-bar percentage=health />
                           </div>
                         </div>
                         <span class="badge bg-light text-dark">,(format nil "~a%" (round health))</span>
                       </div>)
               <div class="mt-3">
                 <a href="/channels/new" class="btn btn-outline-primary btn-sm">
                   <mdi name="add" class="me-1" />
                   Add Channel
                 </a>
               </div>
             </div>
             <div class="text-center py-4 text-muted">
               <mdi name="book" style="font-size: 3rem;" class="mb-2" />
               <p>No channels found</p>
               <a href="/channels/new" class="btn btn-primary">Create your first channel</a>
             </div>)
        </div>
      </div>
    </div>
    <div class="col-md-4">
      <div class="card channel-grid-card attention-required">
        <div class="card-header">
          <h5 class="mb-0">
            <mdi name="warning" class="me-2 text-warning" />
            Attention Required
          </h5>
        </div>
        <div class="card-body">
          ,(let ((problem-channels (remove-if (lambda (channel)
                                               (>= (calculate-channel-health channel) 80))
                                             channels)))
             (if problem-channels
               <div>
                 ,@(loop for channel in (subseq problem-channels 0 (min 3 (length problem-channels)))
                         for health = (calculate-channel-health channel)
                         collect
                         <div class="alert alert-warning mb-2 py-2">
                           <div class="d-flex align-items-center">
                             <mdi name="error_outline" class="me-2" />
                             <div>
                               <h6 class="mb-1">,(channel-name channel)</h6>
                               <small>Health: ,(format nil "~a%" (round health))</small>
                             </div>
                           </div>
                         </div>)
               </div>
               <div class="text-center text-muted">
                 <mdi name="check_circle" class="text-success mb-2" style="font-size: 2rem;" />
                 <p>All channels healthy!</p>
               </div>))
        </div>
      </div>
    </div>
  </div>)

(defun dashboard-view ()
  "Main dashboard view"
  (assert (gk:check :new-dashboard-view (auth:current-company)))
  (let* ((company (current-company))
         (metrics (calculate-dashboard-metrics company)))
    <app-template>
      <div class="main-content">
        <div class="container-fluid mt-4 dashboard-overview">
          <div class="d-flex justify-content-between align-items-center mb-4">
            <h2>
              <mdi name="dashboard" class="me-2" />
              Dashboard Overview
            </h2>
            <a href="/settings/general" class="btn btn-outline-secondary">
              <mdi name="settings" class="me-1" />
              Settings
            </a>
          </div>
          
          <dashboard-overview-cards metrics=metrics />
          
          <activity-feed recent-activity=(getf metrics :recent-activity) />
          
          <channel-status-grid channels=(getf metrics :channels) />
        </div>
      </div>
    </app-template>))

(defhandler (dashboard-page :uri "/dashboard") ()
  (with-login ()
    (dashboard-view)))

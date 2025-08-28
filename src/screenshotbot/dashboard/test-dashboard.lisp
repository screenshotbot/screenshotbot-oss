;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-dashboard
  (:use #:cl
        #:alexandria
        #:fiveam
        #:screenshotbot/dashboard/dashboard)
  (:import-from #:screenshotbot/testing
                #:fix-timestamps
                #:with-installation
                #:screenshot-test
                #:with-test-user)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/model/user
                #:user)
  (:import-from #:screenshotbot/model/report
                #:report
                #:base-acceptable
                #:acceptable-history
                #:acceptable-history-item-state
                #:acceptable-history-item-user
                #:acceptable-history-item-ts
                #:acceptable-history-item)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:make-instance)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/dashboard/dashboard
                #:activity-feed
                #:get-recent-activity))
(in-package :screenshotbot/dashboard/test-dashboard)

(util/fiveam:def-suite)

(named-readtables:in-readtable markup:syntax)

(defun create-test-activity-data (company)
  "Create test acceptables with history for the activity feed"
  (let* ((channel1 (make-instance 'channel :name "web-frontend" :company company))
         (channel2 (make-instance 'channel :name "mobile-app" :company company))
         (user1 (make-instance 'user 
                               :full-name "John Doe"
                               :email "john@example.com"))
         (user2 (make-instance 'user 
                               :full-name "Jane Smith" 
                               :email "jane@example.com"))
         (run1 (make-recorder-run :channel channel1 
                                  :pull-request "https://github.com/example/repo/pull/123"))
         (run2 (make-recorder-run :channel channel2 
                                  :pull-request "https://github.com/example/mobile/pull/456"))
         (run3 (make-recorder-run :channel channel1))
         (report1 (make-instance 'report :run run1 :channel channel1))
         (report2 (make-instance 'report :run run2 :channel channel2))
         (report3 (make-instance 'report :run run3 :channel channel1)))
    
    ;; Create acceptables with history
    (let ((acceptable1 (make-instance 'base-acceptable :report report1))
          (acceptable2 (make-instance 'base-acceptable :report report2))
          (acceptable3 (make-instance 'base-acceptable :report report3)))
      
      ;; Add history items with different timestamps
      (with-transaction ()
        (setf (acceptable-history acceptable1)
              (list (make-instance 'acceptable-history-item
                                   :state :accepted
                                   :user user1
                                   :ts (- (get-universal-time) 3600)) ; 1 hour ago
                    (make-instance 'acceptable-history-item
                                   :state :rejected
                                   :user user2
                                   :ts (- (get-universal-time) 7200)))) ; 2 hours ago
        
        (setf (acceptable-history acceptable2)
              (list (make-instance 'acceptable-history-item
                                   :state :rejected
                                   :user user1
                                   :ts (- (get-universal-time) 1800)))) ; 30 minutes ago
        
        (setf (acceptable-history acceptable3)
              (list (make-instance 'acceptable-history-item
                                   :state :accepted
                                   :user user2
                                   :ts (- (get-universal-time) 600))))) ; 10 minutes ago
    
    (values company (list channel1 channel2)))))

(defun render-activity-feed-only (company)
  "Render just the activity feed component wrapped in app template"
  (let ((recent-activity (get-recent-activity company)))
    <app-template>
      <link rel="stylesheet" href="/css/dashboard.css" />
      <div class="container-fluid mt-4">
        <h2 class="mb-4">Activity Feed Test</h2>
        <activity-feed recent-activity=recent-activity />
      </div>
    </app-template>))

(screenshot-test activity-feed-screenshot
  (fix-timestamps
   (with-installation ()
     (with-test-store ()
       (with-test-user (:company company :logged-in-p t)
         (multiple-value-bind (test-company channels) (create-test-activity-data company)
           ;; Set the current company to our test company for the activity feed
           (setf (screenshotbot/user-api:current-company) test-company)
          
           (render-activity-feed-only test-company)))))))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/recent-runs
  (:use #:cl
        #:alexandria
        #:screenshotbot/user-api
        #:screenshotbot/dashboard/numbers
        #:screenshotbot/template
        #:screenshotbot/taskie
        #:markup)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util #:make-url #:oid)
  (:import-from #:screenshotbot/ui
                #:ui/a
                #:ui/div)
  (:import-from #:screenshotbot/dashboard/run-page
                #:commit)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-page)
  (:import-from #:screenshotbot/model/recorder-run
                #:phabricator-diff-id)
  (:export #:recent-runs))
(in-package :screenshotbot/dashboard/recent-runs)

(markup:enable-reader)

(hex:declare-handler 'run-page)

(defun find-recent-runs (&key user user-id)
  (sort (company-runs (current-company :user user))
        'local-time:timestamp>
        :key 'created-at))

(deftag conditional-commit (&key repo hash)

  (cond
    ((and repo hash)
       <span>on <commit repo= repo
                        hash= hash /></span>)
    (t
     nil)))

(deftag recorder-run-row (&key run)
  (taskie-row :object run
    (ui/a :href (make-url 'run-page :id (oid run))
      (channel-name (recorder-run-channel run)))
    (ui/div
      (cond
        ((activep run)
         <span>
           Promoted run
           <conditional-commit repo= (channel-repo (recorder-run-channel run))
                               hash= (recorder-run-commit run) />
         </span>)
        ((recorder-previous-run run)
         <span>Previously promoted run
           <conditional-commit repo= (channel-repo (recorder-run-channel run))
                               hash= (recorder-run-commit run) />
         </span>)
        ((pull-request-url run)
         <span>
           Run on <a href= (pull-request-url run)>
             ,(cond
                ((phabricator-diff-id run)
                 <span>Revision</span>)
                (t
                 <span>Pull Request</span>))
                  </a>
         </span>)
        (t
         <span>
           Unpromoted run
           ,(when-let ((repo (channel-repo (recorder-run-channel run)))
                       (hash (recorder-run-commit run)))
              <span>
                on
                <commit repo= repo hash=hash />
              </span>)
         </span>
)))

    (taskie-timestamp :prefix "" :timestamp (created-at run))))

(defun render-recent-runs (runs &key (user (current-user))
                                  (numbersp t)
                                  (check-access-p t)
                                  (script-name (hunchentoot:script-name*))
                                  (company (current-company)))
  (with-pagination (runs runs :next-link next-link
                              :prev-link prev-link)
    (when check-access-p
     (apply 'can-view! runs))
    (dashboard-template
     :user user
     :company company
     :script-name script-name
     (when numbersp
       (numbers-section :company company))
     (taskie-page-title :title "Recent Runs"
                        (ui/a :id "compare-runs" :btn :success
                              :class "btn-sm ms-3"
                              "Compare")
                        (ui/a :id "delete-runs" :btn :danger
                              :class "btn-sm ms-1"
                              "Delete Selected"))

     (taskie-list :empty-message "No recent runs to show. But that's okay, it's easy to get started!"
                  :items runs
                  :next-link next-link
                  :prev-link prev-link
                  :row-generator (lambda (run)
                                   (recorder-run-row :run run))))))


(defhandler (recent-runs :uri "/runs") ()
  (needs-user!)

  (log:info "Current user is: ~s" (current-user))
  (let ((runs (find-recent-runs :user (current-user))))
    (render-recent-runs runs)))

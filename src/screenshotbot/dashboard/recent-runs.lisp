;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/recent-runs
  (:use #:cl
        #:alexandria
        #:screenshotbot/user-api
        #:screenshotbot/template
        #:core/ui/taskie
        #:markup)
  (:import-from #:screenshotbot/server
                #:with-login
                #:defhandler)
  (:import-from #:util #:make-url #:oid)
  (:import-from #:screenshotbot/ui
                #:ui/a
                #:ui/div)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-link
                #:commit)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-page)
  (:import-from #:screenshotbot/dashboard/explain
                #:explain)
  (:import-from #:screenshotbot/installation
                #:default-logged-in-page)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/dashboard/review-link
                #:review-link)
  (:import-from #:screenshotbot/model/recorder-run
                #:runs-for-tag
                #:runs-for-company)
  (:export #:recent-runs))
(in-package :screenshotbot/dashboard/recent-runs)

(markup:enable-reader)

(hex:declare-handler 'run-page)

(defun find-recent-runs ()
  (runs-for-company (current-company)))

(deftag conditional-commit (&key repo hash)

  (cond
    ((and repo hash)
       <span>on <commit repo= repo
                        hash= hash /></span>)
    (t
     nil)))

(deftag promoted-tooltip ()
  <div>
    <p>
      A <b>promoted</b> run is the current "golden" run on your master branch.
    </p>

    <p>Promotion logic is used to send notifications for changes on your master branch,
      and to track the history of a given screenshot. For projects not associated with a
      repository, the promoted run is usually the most recent run.</p>

    <p>Promoted runs are <b>not</b> used for determining changes on Pull Requests. For that we just use the first known run on the merge-base.</p>

  </div>)

(deftag recorder-run-row (&key run)
  (taskie-row :object run
              (ui/a :href
                    (format nil "/runs/~a" (oid run))
                    (channel-name (recorder-run-channel run)))
              (ui/div
               (let ((review-link (review-link :run run)))
                 (cond
                   ((activep run)
                    <span>
                    Promoted<explain title= "Promoted run"><promoted-tooltip /></explain>  run
                    <conditional-commit repo= (channel-repo (recorder-run-channel run))
                    hash= (recorder-run-commit run) />
                    </span>)
                   ((recorder-previous-run run)
                    <span>Previously promoted run
                    <conditional-commit repo= (channel-repo (recorder-run-channel run))
                    hash= (recorder-run-commit run) />
                    </span>)
                   (review-link
                    <span>
                    Run on ,(progn review-link)
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
                    ))))

              (taskie-timestamp :prefix "" :timestamp (created-at run))))

(defun render-recent-runs (runs &key (user (current-user))
                                (title "Recent Runs")
                                  (check-access-p t)
                                  (script-name (hunchentoot:script-name*))
                                  (company (current-company)))
  (with-pagination (runs runs :next-link next-link
                              :prev-link prev-link)
    (when check-access-p
     (apply 'can-view! runs))
    (dashboard-template
     :user user
     :title "Screenshotbot: Runs"
     :company company
     :script-name "/runs"
     (taskie-page-title :title title
                        (ui/a :id "delete-runs" :btn :danger
                              :class "btn-sm"
                              "Delete Selected")
                        (ui/a :id "compare-runs" :btn :success
                              :class "btn-sm ms-1"
                              "Compare"))

     (taskie-list :empty-message "No recent runs to show. But that's okay, it's easy to get started!"
                  :items runs
                  :headers (list "Channel" "Status" "Date")
                  :next-link next-link
                  :prev-link prev-link
                  :row-generator (lambda (run)
                                   (recorder-run-row :run run))))))

(defun %recent-runs ()
  (with-login ()
   (let ((runs (find-recent-runs)))
     (render-recent-runs runs))))

(defhandler (recent-runs :uri "/runs") ()
  (%recent-runs))

(defmethod default-logged-in-page ((installation t))
  (%recent-runs))

(defhandler (nil :uri "/runs/by-tag/:tag") (tag)
  (with-login ()
   (let ((runs (runs-for-tag (current-company) tag)))
     (cond
       ((= 1 (length runs))
        (hex:safe-redirect
         (run-link (car runs))))
       (t
        (render-recent-runs runs))))))

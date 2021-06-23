;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/numbers
    (:use #:cl
          #:alexandria
          #:../user-api)
  (:import-from #:markup
                #:deftag)
  (:import-from #:../template
                #:mdi)
  (:export #:numbers-section))

(markup:enable-reader)

(deftag numbers-card (&key title value last-week-value)
  (let* ((change (if (> last-week-value 0)
                     (- (* 100 (/ value last-week-value)) 100)
                     0.0))
         (pos (>= change 0.0)))
    <div class= "col-xl-3 col-lg-4 col-md-4 mt-3 mb-3">
      <div class= "card">
        <div class= "card-body">
          <div class= "float-end">
            <mdi name= "leaderboard" />
          </div>
          <h5 class= "text-muted font-weight-normal mt-0">,(progn title)</h5>
          <h3 class= "mt-3 mb-3"> ,(format nil "~:d" value)</h3>
          <p class= "mb-0 text-muted" >
            <span class= (format nil "~a mr-2" (if pos "text-success" "text-danger")) >
              <mdi name= (if pos "trending_up" "trending_down") />
              ,(format nil "~1$" change)%
            </span>
            <span class= "text-nowrap" >Since last week</span>
          </p>
        </div>
      </div>
    </div>))

(deftag numbers-section (&key company)
  (let* ((now (local-time:now))
         (last-week (local-time:timestamp- now 7 :day))
         (14-days-ago (local-time:timestamp- now 14 :day))
         (this-week-screenshots (num-screenshots-last-week company
                                                       last-week
                                                       now))
         (last-week-screenshots (num-screenshots-last-week company
                                                           14-days-ago
                                                           last-week)))
    <div class= "row">
      ,(numbers-card :title "Screenshots"
                     :value this-week-screenshots
                     :last-week-value last-week-screenshots)

      ,(numbers-card :title "Changes"
                     :value (num-changes-in-period company last-week now)
                     :last-week-value (num-changes-in-period company 14-days-ago last-week))
    </div>))

(defun num-changes-in-period (company start-time end-time)
  (loop for report in (company-reports company)
        if (and
            (local-time:timestamp< start-time (created-at report) end-time))
          summing (or (report-num-changes report) 0)))

(defun num-screenshots-last-week (company
                                  start-time
                                  end-time)
  (num-screenshots-in-runs
   (loop for run in (company-runs company)
         if (and
             (local-time:timestamp< start-time (created-at run) end-time))
           collect run)))

(defun num-screenshots-in-runs (runs)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (run runs)
      (dolist (screenshot (recorder-run-screenshots run))
        (setf (gethash (cons
                        (recorder-run-channel run)
                        (screenshot-name screenshot))
                       table)
              t)))
    (hash-table-count table)))

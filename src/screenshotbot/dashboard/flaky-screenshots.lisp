;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/flaky-screenshots
  (:use #:cl)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:screenshotbot/model/recorder-run
                #:runs-for-channel)
  (:import-from #:local-time
                #:timestamp-)
  (:import-from #:screenshotbot/user-api
                #:screenshot-name
                #:recorder-run-screenshots
                #:created-at)
  (:import-from #:screenshotbot/screenshot-api
                #:screenshot-image)
  (:import-from #:alexandria
                #:hash-table-keys
                #:hash-table-alist)
  (:import-from #:screenshotbot/dashboard/compare
                #:screenshot-box)
  (:import-from #:core/ui/paginated
                #:paginated)
  (:export
   #:view-flaky-screenshots))
(in-package :screenshotbot/dashboard/flaky-screenshots)

(named-readtables:in-readtable markup:syntax)


(defun view-flaky-screenshots (channel)
  "A view of potential flaky screenshots in the channel."
  (auth:can-view! channel)
  (let ((map (screenshot-variant-map (runs-from-30-days channel))))
    <app-template>
      <div class= "main-content mt-3">
        <h3>Screenshots that have changed frequently in the last 30 days</h3>
        <div class= "alert alert-info">
          This is useful for debugging flaky screenshots. For each screenshot name, we
          list the number of unique screenshots we have seen for it over the last 30 days.
          <b>Masks are not factored into this count</b>, i.e. if you have a screenshot with mask
          each variant will get counted here even if changes were in a masked area.
        </div>
        <div>
          ,(paginated
            (lambda (pair)
              (destructuring-bind (name . screenshots) pair
                <div>
                  ,(progn name) (variants: ,(hash-table-count screenshots))
                  <screenshot-box image= (car (hash-table-keys screenshots)) />
                </div>))
            :items map)

        </div>
      </div>
    </app-template>))

(defun screenshot-variant-map (runs)
  (let ((map (make-hash-table :test #'equal)))
    (dolist (run runs)
      (loop for screenshot in (recorder-run-screenshots run) do
        (let ((name (screenshot-name screenshot)))
          (unless (gethash name map)
            (setf (gethash name map) (make-hash-table :test #'equal)))
          (setf (gethash (screenshot-image screenshot)
                         (gethash name map))
                t))))
    (sort
     (hash-table-alist map)
     #'>
     :key (lambda (pair)
                        (hash-table-count (cdr pair))))))



(defun runs-from-30-days (channel)
  (let ((runs (runs-for-channel channel))
        (cutoff (timestamp- (local-time:now) 30 :day)))
    (loop for rank from (1- (fset:size runs)) above -1
          for run = (fset:at-rank runs rank)
          if (local-time:timestamp<
              (created-at run)
              cutoff)
                     do (return result)
          else 
            collect run into result)))


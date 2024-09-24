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
                #:when-let
                #:hash-table-keys
                #:hash-table-alist)
  (:import-from #:screenshotbot/dashboard/compare
                #:screenshot-box)
  (:import-from #:core/ui/paginated
                #:paginated)
  (:import-from #:screenshotbot/diff-report
                #:after
                #:diff-report-changes
                #:make-diff-report)
  (:import-from #:screenshotbot/report-api
                #:report-previous-run
                #:report-run)
  (:import-from #:screenshotbot/model/report
                #:reports-for-run)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:nibble
                #:nibble)
  (:export
   #:view-flaky-screenshots))
(in-package :screenshotbot/dashboard/flaky-screenshots)

(named-readtables:in-readtable markup:syntax)


(defun view-noisy-screenshots (channel)
  "A view of potential flaky screenshots in the channel."
  (auth:can-view! channel)
  (let ((map (report-count-map (runs-from-30-days channel))))
    <app-template>
      <div class= "main-content mt-3">
        <h3>Noisy screenshots over the last 7 days</h3>

        <p>See also: <a href= (nibble () (view-flaky-screenshots channel)) >Screenshots with animations</a></p>
        <div class= "alert alert-info">
          This is useful for debugging flaky screenshots. For each screenshot name, we track the
          number of times it showed up as changed on a PR report or main-branch report. 
        </div>
        <div>
          ,(paginated
            (lambda (pair)
              (destructuring-bind (name . samples) pair
                (destructuring-bind (image sample-report) (car samples)
                    <div>
                      ,(progn name) (Occurrences: ,(length samples), <a href= (hex:make-url "/report/:id" :id (oid sample-report)) >Sample Report</a>)
                      <screenshot-box image= image />
                    </div>)))
            :items map)

        </div>
      </div>
    </app-template>))

(defun view-flaky-screenshots (channel)
  "A view of potential flaky screenshots in the channel."
  (auth:can-view! channel)
  (let ((map (screenshot-variant-map (runs-from-30-days channel))))
    <app-template>
      <div class= "main-content mt-3">
        <h3>Screenshots with animations </h3>

        <p>See also: <a href= (nibble () (view-noisy-screenshots channel)) >Noisy screenshots</a></p>

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
                  ,(progn name) (variants: ,(length screenshots))
                  <screenshot-box image= (car screenshots) />
                </div>))
            :items map)

        </div>
      </div>
    </app-template>))

(defun report-count-map (runs)
  (let ((map (make-hash-table :test #'equal)))
    (dolist (run runs)
      (when-let ((report (car (reports-for-run run))))
        (let ((diff-report (make-diff-report
                            (report-run report)
                            (report-previous-run report))))
          (loop for change in (diff-report-changes diff-report)
                do
                   (push (list (screenshot-image (after change)) report)
                         (gethash (screenshot-name (after change))
                                  map))))))
    (sort
     (hash-table-alist map)
     #'>
     :key (lambda (pair)
            (length (cdr pair))))))

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
    (remove-if
     (lambda (pair)
       (= 1 (length (cdr pair))))
     (sort
      (loop for key being the hash-keys of map
            using (hash-value v)
            collect (cons key (hash-table-keys v)))
      #'>
      :key (lambda (pair)
             (length (cdr pair)))))))



(defun runs-from-30-days (channel)
  (let ((runs (runs-for-channel channel))
        (cutoff (timestamp- (local-time:now) 7 :day)))
    (loop for rank from (1- (fset:size runs)) above -1
          for run = (fset:at-rank runs rank)
          if (local-time:timestamp<
              (created-at run)
              cutoff)
                     do (return result)
          else 
            collect run into result)))


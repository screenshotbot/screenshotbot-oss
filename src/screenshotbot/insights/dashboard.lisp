;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/dashboard
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:ps
                #:@)
  (:import-from #:core/active-users/active-users
                #:format-date
                #:active-user-date
                #:active-users-for-company)
  (:import-from #:screenshotbot/login/common
                #:with-login)
  (:import-from #:screenshotbot/user-api
                #:company-name)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/model/company
                #:has-root-company-p)
  (:import-from #:screenshotbot/insights/runs
                #:active-screenshot-key-screenshot-key
                #:active-screenshot-key-date
                #:active-screenshot-keys)
  (:import-from #:screenshotbot/insights/pull-requests
                #:pr-to-actions)
  (:import-from #:core/ui/taskie
                #:taskie-page-title)
  (:local-nicknames (#:active-users
                     #:core/active-users/active-users)))
(in-package :screenshotbot/insights/dashboard)

(named-readtables:in-readtable markup:syntax)

(defclass dataset ()
  ((label :initarg :label
          :reader dataset-label)
   (data-labels :initarg :data-labels
                :reader dataset-data-labels
                :documentation "A hash-table. The keys are the keys
provided to generate-chart, and the value is the label we will show")
   (data :initarg :data
         :reader dataset-data
         :documentation "A hash-table. The keys are the keys provided to generate-chart."))
  (:metaclass ext-json-serializable-class))


(defun generate-chart-on-canvas (canvas-name &key keys
                                               (default-value 0)
                                               (data-labels nil)
                                               (background-colors nil)
                                               (title "No title")
                                               (type "line")
                                               datasets)
  "DEFAULT-VALUE is the value used if the data for the key is not present in data."
  (ps:ps
    (funcall
     (lambda ()
       (let* ((ctx ((@ document get-element-by-id)  (ps:lisp canvas-name)))
              (datasets ((@ -J-S-O-N parse) (ps:lisp
                                             (json:encode-json-to-string datasets))))
              (labels (ps:lisp `(list ,@keys)))
              (parsed-datasets (loop for dataset in datasets
                                   collect
                                   (ps:create
                                    :label (@ dataset label)
                                    :data (loop for x in labels
                                                collect
                                                (or (aref (@ dataset data) x)
                                                    (ps:lisp default-value)))
                                    "backgroundColor" (ps:lisp (if background-colors `(list ,@background-colors)))
                                    :cubic-interpolation-mode "monotone"
                                    :tension 0.2
                                    :border-width 1))))
         (ps:new
          (-Chart
           ctx
           (ps:create
            :type (ps:lisp type)
            :data (ps:create
                   :labels labels
                   :datasets parsed-datasets)
            :plugins (ps:lisp (if data-labels `(list -Chart-Data-Labels)))
            :options (ps:create
                      :layout (ps:create
                               :padding 25)
                      :responsive t
                      :tooltips (ps:create
                                 :enabled t)
                      :plugins (ps:create
                                :title (ps:create
                                        :display t
                                        :text (ps:lisp title))
                                :datalabels (ps:create
                                             :formatter (lambda (value ctx)
                                                          ((@ console log) "ctx is" ctx)
                                                          (let ((key (aref labels (@ ctx data-index)))
                                                                ;; Which dataset are we looking at? For a Pie chart this will typically be zero.
                                                                (dataset-index (@ ctx dataset-index)))
                                                            (let ((data-labels (@ (aref datasets dataset-index) data-labels)))
                                                              ((@ console log) "data labels map is" data-labels)
                                                             (aref
                                                              data-labels
                                                              (@ ctx data-index)))))))
                      :scales (unless (equal "pie" (ps:lisp type))
                                  (ps:create
                                      :y (ps:create
                                          "beginAtZero" t))))))))))))

;; (generate-chart-on-canvas "foo")

(defun daily-active-users (active-users)
  (weekly-active-users active-users :trail-size 1))

(defun last-30-days (&key (n 30))
  (let ((now (get-universal-time)))
    (reverse
     (loop for i from 0 to n
           collect (format-date (- now (* i 24 3600)))))))

(defun last-60-days ()
  (last-30-days :n 60))


(defun n-day-active-count (active-objects ;; Should store <date> and the actual object
                           &key trail-size
                             value-accessor
                             date-accessor)
    (let ((day-map
          ;; A map from day to list of users (not active-user objs)
          (make-hash-table :test #'equal)))
    (loop for active-user in active-objects
          do (push (funcall value-accessor active-user) (gethash (funcall date-accessor active-user) day-map nil)))

    (let ((current-users (make-hash-table :test #'equal))
          (ans (make-hash-table :test #'equal)))
      (loop for day in (last-60-days)
            for 7-days-ago  in (append
                                (loop for i from 0 upto trail-size collect nil)
                                (last-60-days))

            do
               (loop for user in (gethash day day-map)
                     do (incf (gethash user current-users 0)))
               (loop for user in (gethash 7-days-ago day-map)
                     do
                        (decf (gethash user current-users))
                        (when (= 0 (gethash user current-users))
                          (remhash user current-users)))
               (setf (gethash day ans) (hash-table-count current-users)))
      ans)))

(defun weekly-active-users (active-users &key (trail-size 7 #| week by default |#))
  "There are certainly more efficient ways of doing this, but doesn't
matter. The code was initially hard-coded for trail-size=7,
i.e. weekly-active, hence the name, but it can also be used for
monthly-active."
  (n-day-active-count active-users
                      :trail-size trail-size
                      :value-accessor #'active-users::user
                      :date-accessor #'active-user-date))

(defun monthly-active-users (active-users)
  (weekly-active-users active-users :trail-size 30))

(defun generate-daily-active-users (company id)
  (let* ((active-users (active-users-for-company company :company-test #'has-root-company-p))
         (data (daily-active-users active-users)))
    (generate-chart-on-canvas id
                              :keys (last-30-days)
                              :title (format nil "Active Users on ~a" (company-name company))
                              :datasets
                              (list
                               (make-instance 'dataset
                                              :label "Daily Active"
                                              :data data)
                               (make-instance 'dataset
                                              :label "Weekly Active (trailing)"
                                              :data (weekly-active-users active-users))
                               (make-instance 'dataset
                                              :label "Monthly Active (trailing)"
                                              :data (monthly-active-users active-users))))))

(defun generate-active-screenshots (company id)
  (let* ((active-screenshots  (active-screenshot-keys company))
         (data (n-day-active-count active-screenshots
                                   :trail-size 30
                                   :date-accessor #'active-screenshot-key-date
                                   :value-accessor #'active-screenshot-key-screenshot-key)))
    (generate-chart-on-canvas id
                              :keys (last-30-days)
                              :title (format nil "Active screenshots over the last 30 days")
                              :datasets
                              (list
                               (make-instance 'dataset
                                              :label "Screenshots"
                                              :data data)))))


(defun generate-pull-requests-chart (company id)
  (let ((no-action "PRs with no action on Screenshotbot")
        (rejected "PRs with at least one rejection")
        (accepted "PRs with only accepted screenshots"))
   (let ((data (make-hash-table :test #'equal)))
     (loop for action being the hash-values of (pr-to-actions company)
           do
              (ecase action
                (:accepted
                 (incf (gethash accepted data 0)))
                (:rejected
                 (incf (gethash rejected data 0)))
                (:none
                 (incf (gethash no-action data 0)))))
     (flet ((pct (label)
              (format nil "~,1f%" (* 100
                                    (/ (gethash label data 0)
                                       (max (loop for val being the hash-values of data
                                                  summing val)
                                            1))))))
      (generate-chart-on-canvas id
                                :type "pie"
                                :keys (list no-action
                                            rejected
                                            accepted)
                                :title "Percentage of PRs with activity on Screenshotbot"
                                :data-labels t

                                ;; ChartJS brand colors taken from: https://github.com/chartjs/Chart.js/blob/ea88dba68d41d4974c1fff5ce1c60f5d68279c13/docs/scripts/utils.js#L127
                                :background-colors (list "rgb(54, 162, 235)" "rgb(255, 99, 132)" "rgb(75, 192, 192)" )
                                :datasets
                                (list
                                 (make-instance 'dataset
                                                :label "Number of PRs"
                                                :data-labels (list
                                                              (pct no-action)
                                                              (pct rejected)
                                                              (pct accepted))
                                                :data  data)))))))

(easy-macros:def-easy-macro script-tag (&fn fn)
  <script async= "async" type= "text/javascript" src=
          (nibble ()
                    (fn))
          />)

(defun render-analytics (company)
  (auth:can-view! company)
  <app-template>

    <div class= "container">

      ,(taskie-page-title :title "Insights")
      <div class= "main-content">
        <p class= "" >We provide these Insights to help you quantify the impact of your screenshot tests. These metrics are computed in near real time, with just a few minutes lag.</p>

        <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
        <script src= "https://cdn.jsdelivr.net/npm/chartjs-plugin-datalabels@2" />

        <div class= "row analytics-row mt-3 mb-3 pt-3 g-3">
          <div class= "col-md-6">
            <div class= "chart-container" >
              <canvas id="myChart"></canvas>
            </div>
          </div>

          <div class= "col-md-6">
            <div class= "chart-container" >
              <canvas id= "active-screenshots" />
            </div>
          </div>


          <div class= "col-md-6">
            <div class= "chart-container" >
              <canvas id= "pull-requests" />
            </div>
          </div>
        </div>
      </div>
    </div>


    ,(script-tag ()
       (generate-daily-active-users company "myChart"))

    ,(script-tag ()
       (generate-active-screenshots company "active-screenshots"))

    ,(script-tag ()
       (generate-pull-requests-chart company "pull-requests"))
  </app-template>)

(defhandler (nil :uri "/insights") ()
  (with-login ()
    (render-analytics (auth:current-company))))
;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/analytics-dashboard/dashboard
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
                #:with-login))
(in-package :screenshotbot/analytics-dashboard/dashboard)

(named-readtables:in-readtable markup:syntax)



(defun generate-chart-on-canvas (canvas-name &key keys
                                               (default-value 0)
                                               data #| hash table |#)
  "DEFAULT-VALUE is the value used if the data for the key is not present in data."
  (ps:ps
    (funcall
     (lambda ()
       (let ((ctx ((@ document get-element-by-id)  (ps:lisp canvas-name))))
         (ps:new
          (-Chart
           ctx
           (ps:create
            :type "bar"
            :data (ps:create
                   :labels (ps:lisp `(list ,@keys))
                   :datasets (list
                              (ps:create
                               :label "# of votes"
                               :data (ps:lisp
                                      `(list ,@(loop for key in keys
                                                     collect (gethash key data default-value))))
                               :border-width 1)))
            :options (ps:create
                      :responsive t
                      :scales (ps:create
                               :y (ps:create
                                   :begin-at-zero t)))))))))))

;; (generate-chart-on-canvas "foo")

(defun daily-active-users (company)
  (let ((active-users (active-users-for-company company))
        (map (make-hash-table :test #'equal)))
    (loop for active-user in active-users
          do (incf (gethash (active-user-date active-user) map 0)))
    map))

(defun last-30-days ()
  (let ((now (get-universal-time)))
    (reverse
     (loop for i from 0 to 30
           collect (format-date (- now (* i 24 3600)))))))

(defun generate-daily-active-users (company id)
  (let ((data (daily-active-users company)))
    (generate-chart-on-canvas id
                              :keys (last-30-days)
                              :data data)))

(defun render-analytics (company)
  (auth:can-view! company)
  <app-template>

    <div>
      <canvas id="myChart"></canvas>
    </div>

    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>

    <script>,(generate-daily-active-users company "myChart")</script>
  </app-template>)

(defhandler (nil :uri "/analytics") ()
  (with-login ()
    (render-analytics (auth:current-company))))

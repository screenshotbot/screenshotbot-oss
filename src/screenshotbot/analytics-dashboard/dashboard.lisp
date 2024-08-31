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
                #:with-login)
  (:import-from #:screenshotbot/user-api
                #:company-name)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:nibble
                #:nibble)
  (:local-nicknames (#:active-users
                     #:core/active-users/active-users)))
(in-package :screenshotbot/analytics-dashboard/dashboard)

(named-readtables:in-readtable markup:syntax)

(defclass dataset ()
  ((label :initarg :label
          :reader dataset-label)
   (data :initarg :data
         :reader dataset-data
         :documentation "A hash-table. The keys are the keys provided to generate-chart."))
  (:metaclass ext-json-serializable-class))


(defun generate-chart-on-canvas (canvas-name &key keys
                                               (default-value 0)
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
            :options (ps:create
                      :responsive t
                      :plugins (ps:create
                                :title (ps:create
                                        :display t
                                        :text (ps:lisp title)))
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

(defun last-30-days (&key (n 30))
  (let ((now (get-universal-time)))
    (reverse
     (loop for i from 0 to n
           collect (format-date (- now (* i 24 3600)))))))

(defun last-60-days ()
  (last-30-days :n 60))

(defun weekly-active-users (company &key (trail-size 7 #| week by default |#))
  "There are certainly more efficient ways of doing this, but doesn't
matter. The code was initially hard-coded for trail-size=7,
i.e. weekly-active, hence the name, but it can also be used for
monthly-active."
  (let ((active-users (active-users-for-company company))
        (day-map
          ;; A map from day to list of users (not active-user objs)
          (make-hash-table :test #'equal)))
    (loop for active-user in active-users
          do (push (active-users::user active-user) (gethash (active-user-date active-user) day-map nil)))

    (let ((current-users (make-hash-table))
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

(defun monthly-active-users (company)
  (weekly-active-users company :trail-size 30))

(defun generate-daily-active-users (company id)
  (let ((data (daily-active-users company)))
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
                                              :data (weekly-active-users company))
                               (make-instance 'dataset
                                              :label "Monthly Active (trailing)"
                                              :data (monthly-active-users company))))))

(defun script-daily-active-users (company id)
  <script type= "text/javascript" src=
          (nibble ()
                    (generate-daily-active-users company id))
          />)


(defun render-analytics (company)
  (auth:can-view! company)
  <app-template>

    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>

    <div>
      <canvas id="myChart"></canvas>
    </div>

    ,(script-daily-active-users company "myChart")
  </app-template>)

(defhandler (nil :uri "/analytics") ()
  (with-login ()
    (render-analytics (auth:current-company))))

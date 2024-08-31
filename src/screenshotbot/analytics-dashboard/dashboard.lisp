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
                #:@))
(in-package :screenshotbot/analytics-dashboard/dashboard)

(named-readtables:in-readtable markup:syntax)

(defun generate-chart-on-canvas (canvas-name)
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
                   :labels (list
                            "Red" "Blue" "yello" "green" "purple" "orange")
                   :datasets (list
                              (ps:create
                               :label "# of votes"
                               :data (list 12 19 3 5 2 3)
                               :border-width 1)))
            :options (ps:create
                      :scales (ps:create
                               :y (ps:create
                                   :begin-at-zero t)))))))))))

;; (generate-chart-on-canvas "foo")

(defhandler (nil :uri "/analytics") ()
  <app-template>

    <div>
      <canvas id="myChart"></canvas>
    </div>

    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>

    <script>,(generate-chart-on-canvas "myChart")</script>
  </app-template>)

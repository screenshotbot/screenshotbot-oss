;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/new-compare
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:dashboard-head
                #:app-template)
  (:import-from #:screenshotbot/diff-report
                #:before
                #:diff-report-changes
                #:make-diff-report)
  (:import-from #:screenshotbot/model/report
                #:report-run
                #:report-previous-run)
  (:import-from #:screenshotbot/screenshot-api
                #:image-public-url
                #:screenshot-image)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:compare-v2-page))
(in-package :screenshotbot/dashboard/new-compare)

(markup:enable-reader)

(defun compare-v2-page (&key report)
  (let* ((diff-report (make-diff-report (report-run report)
                                        (report-previous-run report)))
         (changes (diff-report-changes diff-report)))
    <html>
      <dashboard-head />
      <div class= "split-container">
        <div class= "new-compare split">
          <div class= "image-list" >
            <div style= "max-width:100%"  >
            ,@ (loop for x in changes
                     collect
                     <div>
                       <img src= (image-public-url (screenshot-image (before x)) :size :small)
                            style= "max-width:100%" />
                     </div>)
            </div>
          </div>
          <div class= "image-details">
            world
          </div>
        </div>

        <script src= "/assets/js/dashboard.js" />
      </div>

    </html>))

(defhandler (nil :uri "/new-compare") ()
  (compare-v2-page))

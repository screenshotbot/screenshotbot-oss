;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/history
  (:use #:cl
        #:alexandria
        #:screenshotbot/user-api
        #:screenshotbot/report-api
        #:screenshotbot/template
        #:screenshotbot/screenshot-api)
  (:import-from #:screenshotbot/dashboard/run-page
                #:commit)
  (:import-from #:bknr.datastore
                #:store-object-with-id)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/dashboard/run-page
                #:history-page)
  (:import-from #:screenshotbot/taskie
                #:timeago)
  (:import-from #:util
                #:oid)
  (:import-from #:screenshotbot/dashboard/compare
                #:comparison-modal)
  (:import-from #:screenshotbot/dashboard/paginated
                #:paginated))
(in-package :screenshotbot/dashboard/history)


(markup:enable-reader)

(defun render-single-screenshot (r s &key previous-screenshot
                                       channel)
  (let ((toggle-id (format nil "compare-~a" (random 1000000))))
   (cond
     (s
      <div class= "mb-4" >
      <h4>,(screenshot-name s)</h4>

      ,(when previous-screenshot
         (comparison-modal :toggle-id toggle-id
                           :before previous-screenshot
                           :after s))
      <div class= "screenshot-header">
      <ul class= "screenshot-options-menu">
      <li>
      ,(cond
         ((recorder-run-commit r)
          <span>First seen in <commit repo= (channel-repo channel)
          hash= (recorder-run-commit r) /></span>)
         (t
          <span>First seen <a href= (hex:make-url "/runs/:id" :id (oid r))>,(timeago :timestamp (created-at r))</a></span>))
      </li>

      ,(when previous-screenshot
         <li>
         <a href= "#" data-bs-toggle= "modal" data-bs-target = (format nil "#~a" toggle-id) >  Compare</a>
         </li>)

      </ul>
      </div>
      <a href= (image-public-url (screenshot-image s) :size :full-page) title= "Full screenshot">
      <img src=(image-public-url (screenshot-image s) :size :small) />
      </a>

      </div>)
     (t
      <div>
      <h4>Deleted</h4>
      </div>))))

(markup:deftag render-history (&key screenshot-name channel)
  <div class= "baguetteBox" >
    ,(paginated
      (lambda (args)
        (destructuring-bind (run screenshot previous-screenshot)
            args
          (render-single-screenshot screenshot run
                                    :previous-screenshot previous-screenshot
                                    :channel channel)))
      :num 12
      :iterator (get-screenshot-history channel screenshot-name :iterator t))
  </div>)

(defhandler (history-page :uri "/channel/:channel/history")
            (channel screenshot-name)
  (let ((channel (store-object-with-id (parse-integer channel))))
    (can-view! channel)
    (app-template
     (render-history
      :screenshot-name screenshot-name
      :channel channel))))

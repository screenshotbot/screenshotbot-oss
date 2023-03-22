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
                #:modal-id
                #:render-modal
                #:screenshots-viewer
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
  (:import-from #:core/ui/paginated
                #:paginated)
  (:import-from #:screenshotbot/user-api
                #:screenshot-name)
  (:import-from #:screenshotbot/model/image
                #:image=)
  (:import-from #:screenshotbot/model/image-comparer
                #:make-image-comparer))
(in-package :screenshotbot/dashboard/history)

(named-readtables:in-readtable markup:syntax)

(defun render-single-screenshot (r s &key previous-screenshot
                                       channel)
  (let* ((screenshots-viewer (make-instance 'screenshots-viewer
                                            :navigationp nil
                                            :screenshots (list s)))
         (image-comparer (make-image-comparer r))
         (toggle-id (format nil "compare-~a" (random 1000000)))
         (name-change-p (and
                         previous-screenshot
                         (not (string= (screenshot-name s)
                                       (screenshot-name previous-screenshot)))))
         (image-change-p (and
                          previous-screenshot
                          (not (image=
                                image-comparer
                                (screenshot-image s)
                                (screenshot-image previous-screenshot)
                                nil)))))
   (cond
     (s
      <div class= "mb-4" >
      <h4>,(screenshot-name s)
      ,(when name-change-p
         <span class= "badge bg-warning">
           Renamed or copied
         </span>)
      </h4>

      ,(when (and previous-screenshot image-change-p)
         (comparison-modal :toggle-id toggle-id
                           :before previous-screenshot
                           :after s))
      <div class= "screenshot-header">
      <ul class= "screenshot-options-menu">
      <li>
      ,(cond
         ((recorder-run-commit r)
          <span>First seen in <commit repo= (channel-repo channel)
          hash= (recorder-run-commit r) />, ,(timeago :timestamp (created-at r)) </span>)
         (t
          <span>First seen <a href= (hex:make-url "/runs/:id" :id (oid r))>,(timeago :timestamp (created-at r))</a></span>))
      </li>

      ,(when (and previous-screenshot image-change-p)
         <li>
         <a href= "#" data-bs-toggle= "modal" data-bs-target = (format nil "#~a" toggle-id) >  Compare</a>
         </li>)

      </ul>
      </div>
      ,(render-modal screenshots-viewer)
      <a href= (image-public-url (screenshot-image s) :size :full-page)
         class= "screenshot-run-image"
         title= "Full screenshot"
         data-image-number=0
         data-target= (format nil "#~a" (modal-id screenshots-viewer)) >
      <img class= "screenshot-image" src=(image-public-url (screenshot-image s) :size :small) />
      </a>

      </div>)
     (t
      <div>
      <h4>Deleted</h4>
      </div>))))

(markup:deftag render-history (&key screenshot-name channel)
  <div>
    <h1>Promotion History for ,(progn screenshot-name)</h1>
    ,(paginated
      (lambda (args)
        (destructuring-bind (run screenshot previous-screenshot)
            args
          (assert run)
          <div>
            ,(render-single-screenshot screenshot run
                                       :previous-screenshot previous-screenshot
                                       :channel channel)
          </div>))

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

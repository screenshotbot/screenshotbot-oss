;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/reports
  (:use #:cl
        #:alexandria
        #:screenshotbot/template
        #:screenshotbot/user-api
        #:screenshotbot/report-api
        #:screenshotbot/taskie)
  (:import-from #:screenshotbot/server
                #:with-login
                #:defhandler)
  (:import-from #:util
                #:make-url
                #:find-by-oid
                #:oid)
  (:import-from #:bknr.datastore
                #:store-object-with-id)
  (:import-from #:screenshotbot/dashboard/new-compare
                #:compare-v2-page)
  (:import-from #:screenshotbot/dashboard/notes
                #:render-notes
                #:create-note-page)
  (:import-from #:screenshotbot/installation
                #:installation
                #:installation-domain)
  (:export #:report-page #:report-link))
(in-package :screenshotbot/dashboard/reports)

(markup:enable-reader)

(defhandler (report-page :uri "/report/:id" :method :get) (id)
  (if (member (string id) (list "2" "3") :test 'equal)
      (hex:safe-redirect 'report-page :id (oid (store-object-with-id 814))))
  (if (equal "814" (string id))
      (hex:safe-redirect 'report-page :id (oid (store-object-with-id 814))))
  (let ((report (find-by-oid id)))
    (with-login (:needs-login (not (can-public-view report)))
     (render-report-page report))))

(defun render-report-page (report &rest args &key (lang-filter t)
                                               (device-filter t))
  (flet ((re-run (&rest new-args)
           (apply 'render-report-page
                  report
                  (append new-args args))))
   (let ((lang-filter (make-instance 'row-filter :key 'screenshot-lang
                                                 :value lang-filter))
         (device-filter (make-instance 'row-filter :key 'screenshot-device
                                       :value device-filter)))
     (check-type report report)
     (can-view! report)

     (cond
       ((hunchentoot:parameter "v2")
        (compare-v2-page :report report))
       (t
        <app-template>

          ,(when (and nil (can-public-view report))
             <section class= "mt-3" >
               <div class= "alert alert-danger">
                 This report can be viewed by public, because the underlying repository is public
               </div>
             </section>)

          ,(when nil
             <section class= "mt-3" >
               <div class= "alert alert-info">
                 Hover on the image on the left to visualize the difference!
               </div>
             </section>

             )

          <section class= "full-height">
            ,(render-notes :for report)
            <render-diff-report run= (report-run report) to= (report-previous-run report)
                                lang-filter=lang-filter
                                device-filter=device-filter
                                acceptable= (report-acceptable report)
                                more= (list
                                       (cons
                                         "Add Note"
                                         (create-note-page :for report :redirect (make-url 'report-page :id (oid report)))))
                                re-run=#'re-run />
          </section>
        </app-template>)))))

(defhandler (report-list :uri "/report" :method :get
                         :want-login t) ()
  (flet ((row-generator (row)
           <taskie-row object=row >
             <a href= (make-url 'report-page
                                 :id (oid row))>
                                 ,(channel-name (recorder-run-channel (report-run row)))
             </a>
             <div>,(report-title row) </div>
             <ul class="list-inline font-13 text-right">
               <li class="list-inline-item">
                 <mdi name= "today" />
                 <:time class= "timeago" datetime= (created-at row) >
                   ,(created-at row)
                 </:time>
               </li>
             </ul>
           </taskie-row>))
    (let ((reports (company-reports (current-company))))
      (with-pagination (reports reports
                                :next-link next-link
                                :prev-link prev-link)
        <app-template>
          <div class= "page-title-box">
            <h4 class= "page-title">Recent Reports</h4>
          </div>
          <taskie-list empty-message="No reports to show! Reports are created when
                                      your CI builds create a run with differing images."
                       items=reports
                       next-link=next-link
                       prev-link=prev-link
                       row-generator=#'row-generator
                       />

        </app-template>))))

(defun report-link (report)
  (format nil "~a~a"
          (installation-domain (installation))
          (make-url 'report-page :id (oid report))))

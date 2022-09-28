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
                #:staging-p
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
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:screenshotbot/model/sharing
                #:share-expired-p
                #:share-object
                #:share)
  (:import-from #:util/object-id
                #:oid-array)
  (:export #:report-page #:report-link
           #:shared-report-page))
(in-package :screenshotbot/dashboard/reports)

(markup:enable-reader)

(defhandler (report-page :uri "/report/:id" :method :get) (id)
  (cond
    ((member (string id) (list "2" "3" "5fd16bcf4f4b3822fd000146"
                               "5fd16bcf4f4b3822fd000144"
                               "814")
             :test 'equal)
     (expired-report))
    (t
     (let ((report (ignore-errors (find-by-oid id))))
       (cond
         #-screenshotbot-oss
         ((and
           (staging-p)
           (not report))
          ;; TODO: T389
          ;; We had a temporary bug where we sent out staging links in
          ;; prod, handle this gracefully for now
          (hunchentoot:redirect
           (format nil "https://screenshotbot.io~a" (hunchentoot:script-name*))))
         ((not report)
          ;; We don't use template because this is messing up our Google
          ;; Analytics. This is most likely trigged by Microsoft Outlook's
          ;; preview.
          <html>
          <body>
          Invalid Report link, <a href= "/report">Click here to view recent reports</a>
          </body>
          </html>)
         (t
          (with-login (:needs-login (not (can-public-view report)))
            (render-report-page report))))))))

(defun expired-report ()
  <app-template>
    <div class= "card mt-3" style= "max-width: 40em" >
      <div class= "card-header">
        <h3 class= "mt-0" >Expired report</h3>
      </div>
      <div class= "card-body">
        This report is expired! If you're here from GitHub, please reach out to us at <a href= "mailto:support@screenshotbot.io">support@screenshotbot.io</a> for a more recent demo.
      </div>

      <div class= "card-footer">
        <a href= "/" class = "btn btn-lg btn-primary">Home</a>
      </div>
    </div>
  </app-template>)

(defun render-report-page (report &rest args &key alert skip-access-checks)
  (flet ((re-run (&rest new-args)
           (apply 'render-report-page
                  report
                  (append new-args args))))
   (check-type report report)
    (unless skip-access-checks
     (can-view! report))

    (cond
      ((hunchentoot:parameter "v2")
       (compare-v2-page :report report))
      (t
       <app-template title= (report-title report) >

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
          </section>)

       ,(when alert
          alert)

       <section class= "full-height">
       ,(render-notes :for report)
       <render-diff-report run= (report-run report) to= (report-previous-run report)
       acceptable= (report-acceptable report)
                           more= (remove-if #'null (more-links-for-report report))
       re-run=#'re-run />
       </section>
       </app-template>))))

(defun share-report (report)
  (let ((submit (nibble (expiry-date)
                  (submit-share-report report expiry-date))))
    <simple-card-page form-action=submit >
      <div class= "card-header">
        <h3>Create public link for report</h3>
      </div>

      <div>
        <div class= "alert alert-warning">
          Any person with access to the link will be able to access the report, and all the images associated with it. They will not be able to edit or perform any actions on the report.
        </div>
      </div>

      <div class= "mb-3">
        <label class= "form-label" for= "expiry-date">Expiration date <span class= "text-muted">(leave empty to never expire)</span></label>
        <input type= "date" id= "expiry-date" name= "expiry-date" class= "form-control" />
      </div>

      <div class= "card-footer">
        <input type= "submit" class= "btn btn-primary" value= "Create Public Link" >
          <a href= (report-link report) class= "btn btn-outline-secondary" >Cancel</a>
      </div>
    </simple-card-page>))

(defun submit-share-report (report expiry-date)
  (let ((errors))
    (flet ((check (field check message)
             (unless check
               (push (cons field message) errors))))
      (unless (str:emptyp expiry-date)
        (let ((parsed (local-time:parse-timestring expiry-date)))
          (check :expiry-date parsed "Invalid date")
          (when parsed
            (or
             (check :expiry-date
                    (local-time:timestamp>
                     parsed
                     (local-time:now))
                    "Date can't be in the past")
             (check :expiry-date
                    (local-time:timestamp>
                     parsed
                     (local-time:timestamp+ (local-time:now) 2 :day))
                    "Choose a date at least two days in the future"))))
        (check :expiry-date
               (cl-ppcre:scan "\\d{4}-\\d{2}-\\d{2}"
                expiry-date)
               "Invalid date format, perhaps you're using an old browser? Try YYYY-MM-DD format."))

      (cond
        (errors
         (with-form-errors (:errors errors
                            :was-validated t
                            :expiry-date expiry-date)
           (share-report report)))
        (t
         (let ((share (make-instance 'share
                                      :object report
                                      :creator (current-user)
                                      :company (current-company)
                                      :expiry-date expiry-date)))
           (hex:safe-redirect
            (nibble ()
              (let ((link (hex:make-full-url
                           hunchentoot:*request*
                           'shared-report-page
                            :eoid (encrypt:encrypt-mongoid (oid-array share)))))
               (render-report-page report
                                   :alert
                                   <div class= "alert alert-info mt-3">
                                     <p class= "mb-0" >
                                       Public link to report: <a href=link >,(progn link)</a>
                                     </p>
                                     ,(unless (str:emptyp expiry-date)
                                        <p class= "mb-0" >
                                          This link will expire ,(timeago :timestamp (local-time:parse-timestring expiry-date)).
                                        </p>)
                                   </div>))))))))))

(defun more-links-for-report (report)
  "More links for report. The returned list may have null-values which
 will be ignored."
  (list
   (when (current-user)
     (cons
      <span><mdi name= "share" /> Share</span>
      (nibble (:name "share")
        (share-report report))))
   (when (current-user)
     (cons
      <span><mdi name= "chat" /> Add Note</span>
      (create-note-page :for report :redirect (make-url 'report-page :id (oid report)))))))

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
        <app-template title= "Screenshotbot: Reports" >
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

(defhandler (shared-report-page :uri "/report/:eoid/public") (eoid)
  (let* ((oid (encrypt:decrypt-mongoid eoid))
         (share (find-by-oid oid)))
    (check-type share share)
    (cond
      ((share-expired-p share)
       <app-template>
         <div class= "alert alert-danger mt-3">
           This shared URL has expired.
         </div>
       </app-template>)
      (t
       (let ((report (share-object share)))
         (check-type report report)

         (render-report-page report
                             :skip-access-checks t
                             :alert
                             <div class= "alert alert-warning mt-3">
                             <b>Caution!</b> This is a publicly shared URL of a private report. Some actions on this page will require an authorized logged-in user. <a href= (report-link report)>Click here to view the private report.</a>
                             </div>))))))

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
        #:core/ui/taskie)
  (:import-from #:screenshotbot/server
                #:home-url
                #:staging-p
                #:with-login
                #:defhandler)
  (:import-from #:util
                #:make-url
                #:find-by-oid
                #:oid)
  (:import-from #:bknr.datastore
                #:store-object-with-id)
  (:import-from #:screenshotbot/dashboard/notes
                #:render-notes
                #:create-note-page)
  (:import-from #:screenshotbot/installation
                #:installation
                #:installation-domain)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:screenshotbot/model/sharing
                #:share-expired-p
                #:share-object
                #:share)
  (:import-from #:util/object-id
                #:oid-array)
  (:import-from #:core/ui/taskie
                #:taskie-page-title)
  (:import-from #:screenshotbot/model/report
                #:acceptable-report
                #:acceptable-history-item-state
                #:acceptable-history-item-ts
                #:accepable-history-item-user
                #:acceptable-history-item
                #:acceptable-history
                #:report-company)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:screenshotbot/dashboard/run-page
                #:render-warnings)
  (:import-from #:screenshotbot/user-api
                #:screenshot-name
                #:user-full-name)
  (:import-from #:util/timeago
                #:timeago)
  (:import-from #:screenshotbot/model/company
                #:maybe-redirect-for-company)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/model/screenshot-key
                #:screenshot-key)
  (:import-from #:screenshotbot/dashboard/compare
                #:render-single-change-permalink
                #:render-change-group)
  (:import-from #:screenshotbot/diff-report
                #:make-diff-report)
  (:import-from #:screenshotbot/model/image-comparison
                #:find-image-comparison-on-images)
  (:import-from #:screenshotbot/screenshot-api
                #:screenshot-image)
  (:export #:report-page #:report-link
           #:shared-report-page)
  (:local-nicknames (#:diff-report #:screenshotbot/diff-report)))
(in-package :screenshotbot/dashboard/reports)

(named-readtables:in-readtable markup:syntax)

(def-easy-macro with-report-login (report &fn fn)
  (maybe-redirect-for-company (report-company report))
  (with-login (:needs-login (not (can-public-view report))
               :allow-url-redirect t
                       :company (report-company report))
    (auth:can-view! report)
    (fn)))

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
         ((or
           (not report)
           (not (typep report 'report)))
          (push-event :invalid-report :id id)
          ;; We don't use template because this is messing up our Google
          ;; Analytics. This is most likely trigged by Microsoft Outlook's
          ;; preview.
          <simple-card-page>
            <div>
              Invalid Report link. <a href= "/report">Click here to view recent reports.</a>
            </div>
          </simple-card-page>)
         (t
          (with-report-login (report)
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
        <a href= (home-url) class = "btn btn-lg btn-primary">Home</a>
      </div>
    </div>
  </app-template>)

(defun report-diff-report (report)
  (make-diff-report (report-run report) (report-previous-run report)))

(defun render-report-page (report &key alert skip-access-checks)
  (check-type report report)
  (unless skip-access-checks
    (can-view! report))

  <app-template body-class= "dashboard bg-white" title= (report-title report) >

    ,(when (and nil (can-public-view report))
       <section class= "mt-3" >
         <div class= "alert alert-danger">
           This report can be viewed by public, because the underlying repository is public
         </div>
       </section>)

    ,(when alert
       alert)

    <section class= "full-height">
      ,(render-notes :for report)


      <render-diff-report diff-report= (report-diff-report report)
                          acceptable= (report-acceptable report)
                          more= (remove-if #'null (more-links-for-report report))
                          >
        ,@(render-warnings (report-run report))
      </render-diff-report>
    </section>
  </app-template>)

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
        <input type= "submit" class= "btn btn-primary" value= "Create Public Link" />
        <a href= (report-link report) class= "btn btn-outline-secondary" >Cancel</a>
      </div>
    </simple-card-page>))

(defun submit-share-report (report expiry-date)
  (push-event :share.create)
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
      (create-note-page :for report :redirect (make-url 'report-page :id (oid report)))))
   (when-let ((acceptable (report-acceptable report)))
    (cons
     <span><mdi name= "history"/> Feedback history</span>
     (nibble (:name "review-history")
       (render-acceptable-history acceptable))))
   (when (gk:check :sorted-changes (auth:current-company))
     (cons
      <span><mdi name= "sort" />Sorted by changes</span>
      (format nil "/report/~a/sorted" (oid report))))))

(defun render-acceptable-history (acceptable)
  (let ((history-items (acceptable-history acceptable)))
    <simple-card-page>

      <div class= "card-header" >
        <h4 class= "">
          Review History
        </h4>
      </div>
      <div class= "card-body">
        ,(cond
           (history-items
            <ul>
              ,@ (loop for item in history-items
                       collect (render-acceptable-history-item item))
            </ul>)
           (t
            <span>This report has not been reviewed.</span>))

      </div>
      <div class= "card-footer">
        <a class= "btn btn-link" href= (report-link (acceptable-report acceptable))>Back to report</a>
      </div>
    </simple-card-page>))

(defmethod render-acceptable-history-item ((item acceptable-history-item))
  (let ((class (ecase (acceptable-history-item-state item)
                 (:accepted
                  "text-success")
                 (:rejected
                  "text-danger"))))
    <li>
      <span class=class >,(progn (str:sentence-case (string (acceptable-history-item-state item))))</span>
      by
      ,(user-full-name (accepable-history-item-user item))
      at
      ,(timeago :timestamp (acceptable-history-item-ts item))
    </li>))



(defhandler (nil :uri "/reports" :method :get) ()
  (hex:safe-redirect "/report"))

(defhandler (report-list :uri "/report" :method :get
                         :want-login t) ()
  (auth:can-view! (current-company))
  (flet ((row-generator (row)
           <taskie-row object=row >
             <a href= (make-url 'report-page
                                 :id (oid row))>
                                 ,(channel-name (recorder-run-channel (report-run row)))
             </a>
           <div>,(report-title row) </div>
           <span>
           <mdi name= "today" />
           <:time class= "timeago" datetime= (created-at row) >
           ,(created-at row)
           </:time>
           </span>
           </taskie-row>))
    (let ((reports (company-reports (current-company))))
      (with-pagination (reports reports
                                :next-link next-link
                                :prev-link prev-link)
        <app-template title= "Screenshotbot: Reports" >
          <taskie-page-title title= "Recent Reports" />

          <taskie-list empty-message="No reports to show! Reports are created when
                                      your CI builds create a run with differing images."
                       items=reports
                       headers= (list "Channel" "State" "Time")
                       next-link=next-link
                       checkboxes=nil
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


(defhandler (single-image-page :uri "/report/:oid/image/:key-id") (oid
                                                                key-id)
  (let ((report (util:find-by-oid oid))
        (key-id (parse-integer key-id)))
    (with-report-login (report)
      (let ((diff-report (report-diff-report report)))
        (let ((report-link (format nil "/report/~a" oid)))
          <app-template body-class= "dashboard bg-white">
            ,(render-single-change-permalink diff-report key-id report-link
                                             :run (report-run report))
          </app-template>)))))

(markup:deftag sorted-template (children &key report)
  <app-template>
    <div class= "page-title-box mb-3">
      <div class= "mb-2" ><a href= (report-link report) >Back to report</a></div>
    </div>
    ,@children
  </app-template>)

(defhandler (sorted-by-changes-page :uri "/report/:oid/sorted") (oid)
  (let ((report (util:find-by-oid oid)))
    (with-report-login (report)
      (%render-sorted-by-changes report))))

(defmethod %render-sorted-by-changes ((report report))
  (let* ((diff-report (report-diff-report report))
         (changes (diff-report:diff-report-changes diff-report))
         (comparisons (loop for change in changes
                            collect (find-image-comparison-on-images
                                     (screenshot-image (diff-report:before change))
                                     (screenshot-image (diff-report:after change))
                                     :only-cached-p t))))
    (setf (elt comparisons 0) nil)
    (cond
      ((remove-if-not #'null comparisons)
       <sorted-template report=report >
         <div class= "alert alert-danger mt-2">
           Image processing for this report is not complete yet, so we're unable to show you
           the changes sorted by difference. Please refresh in a few minutes.
         </div>
       </sorted-template>))))



;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/batch
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:with-login
                #:defhandler)
  (:import-from #:util/store/object-id
                #:find-by-oid)
  (:import-from #:screenshotbot/user-api
                #:current-user
                #:channel-name
                #:can-view!)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:screenshotbot/model/batch
                #:batch-item-title
                #:batch-item-status
                #:batch-item-run
                #:batch-item-report
                #:batch-item
                #:batch-item-channel
                #:batch-items)
  (:import-from #:core/ui/taskie
                #:taskie-page-title
                #:taskie-row
                #:taskie-list)
  (:import-from #:core/ui/simple-card-page
                #:confirmation-page)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/model/view
                #:can-edit)
  (:import-from #:screenshotbot/model/report
                #:report-channel
                #:acceptable-state)
  (:import-from #:screenshotbot/report-api
                #:report-acceptable)
  (:import-from #:screenshotbot/model/review-policy
                #:can-review?)
  (:import-from #:screenshotbot/model/channel
                #:review-policy)
  (:import-from #:anaphora
                #:it
                #:acond)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-link)
  (:import-from #:screenshotbot/dashboard/reports
                #:report-link)
  (:import-from #:core/ui/mdi
                #:mdi)
  (:export
   #:batch-handler
   #:sort-items)
  (:local-nicknames (#:batch #:screenshotbot/model/batch)))
(in-package :screenshotbot/dashboard/batch)

(named-readtables:in-readtable markup:syntax)

(defhandler (batch-handler :uri "/batch/:oid") (oid)
  (setf (hunchentoot:header-out :cache-control)
        "no-cache, no-store, must-revalidate")
  (with-login (:allow-url-redirect t)
    (let ((batch (find-by-oid oid)))
      (assert batch)
      (can-view! batch)
      (render-batch batch))))

(defmethod batch-item-link ((item batch-item))
  (acond
    ((batch-item-report item)
     (report-link it))
    ((batch-item-run item)
     (run-link it))
    (t
     (error "no run or report attached to this item"))))

(defun render-batch-item (item)
  (let ((class (ecase (batch-item-status item)
                 (:accepted "success")
                 (:rejected "danger")
                 (:success "muted")
                 (:failure "danger")
                 (:pending "muted")
                 (:action-required "danger")))
        (icon (ecase (batch-item-status item)
                (:accepted "done")
                (:rejected "close")
                (:success "done")
                (:pending "pending")
                (:failure "dangerous")
                (:action-required "report"))))
   (taskie-row
    :object item
    <span>
      <div>
        <mdi name=icon class= (format nil "text-~a" class) />
        ,(let ((link (batch-item-link item))
               (link-class (format nil "link-~a ~a" class (if (eql :rejected (batch-item-status item)) "fw-bold"))))
           (cond
             (link
              <a href= link class= link-class >,(channel-name (batch-item-channel item))</a>)
             (t
              <span>(channel-name (batch-item-channel item))</span>)))
        <span class= "text-muted ms-2" >,(batch-item-title item) </span>
      </div>
    </span>)))

(defparameter *order*
  (list
   :rejected
   :failure
   :action-required
   :accepted
   :pending
   :success))

(defun status-order (item)
  (position (batch-item-status item) *order*))

(defun sort-items (items)
  (sort (copy-list items)
        (lambda (a b)
          (or
           (|<| (status-order a) (status-order b))
           (and
                (= (status-order a) (status-order b))
                (string-lessp
                 (channel-name (batch-item-channel a))
                 (channel-name (batch-item-channel b))))))))

(defun reports-to-accept (batch user)
  "The reports in BATCH that Accept All would accept for USER: those not
already accepted, and that USER is allowed to review. This is the same
pair of checks the Review dropdown on the report page makes, so Accept
All can't do anything that clicking through each report couldn't."
  (when user
    (loop for item in (fset:convert 'list (batch-items batch))
          for report = (batch-item-report item)
          for acceptable = (and report (report-acceptable report))
          for channel = (and report (report-channel report))
          if (and acceptable
                  (not (eql :accepted (acceptable-state acceptable)))
                  (can-edit acceptable user)
                  (or (null channel)
                      (can-review? (review-policy channel) report user)))
            collect report)))

(defun accept-all (batch user)
  "Accepts every report from REPORTS-TO-ACCEPT. The list is recomputed
here instead of being passed in from the page that rendered the link,
since the batch may have changed in between."
  (dolist (report (reports-to-accept batch user))
    (setf (acceptable-state (report-acceptable report) :user user)
          :accepted)))

(defun batch-url (batch)
  (hex:make-url 'batch-handler :oid (util:oid batch)))

(defun confirm-accept-all (batch)
  (let ((count (length (reports-to-accept batch (current-user)))))
    (confirmation-page
     :yes (nibble (:name :batch-accept-all)
            (can-view! batch)
            (accept-all batch (current-user))
            (hex:safe-redirect (batch-url batch)))
     :no (batch-url batch)
     <span>
       Blindly accept ,(progn count) ,(if (eql 1 count) "report" "reports") in this batch,
       without reviewing the changes? This includes any reports that were previously rejected.
     </span>)))

(defmethod render-batch (batch)
  (bt:with-lock-held ((batch:lock batch))
    (let ((items (fset:convert 'list (sort-items (fset:convert 'list (batch-items batch))))))
      <app-template title= "Screenshotbot: Batch" >
        ,(when (reports-to-accept batch (current-user))
           <taskie-page-title title= "Batch" >
             <a href= (nibble (:name :batch-confirm-accept-all)
                        (can-view! batch)
                        (confirm-accept-all batch))
                class= "btn btn-success btn-sm accept-all" >
               <mdi name= "done_all" />
               Blindly Accept All
             </a>
           </taskie-page-title>)
        ,(taskie-list :empty-message "No runs in this batch yet"
                      :items items
                      :headers (list "Channel")
                      :row-generator #'render-batch-item)
      </app-template>)))

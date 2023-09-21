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
  (:import-from #:screenshotbot/taskie
                #:taskie-row
                #:taskie-list)
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
   #:sort-items))
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
  (sort items (lambda (a b)
                (or
                 (|<| (status-order a) (status-order b))
                 (and
                  (= (status-order a) (status-order b))
                  (string-lessp
                   (channel-name (batch-item-channel a))
                   (channel-name (batch-item-channel b))))))))

(defmethod render-batch (batch)
  (let ((items (fset:convert 'list (sort-items (fset:convert 'list (batch-items batch))))))
    <app-template>
      ,(taskie-list :empty-message "No runs in this batch yet"
                    :items items
                    :headers (list "Channel")
                    :row-generator #'render-batch-item)
    </app-template>))

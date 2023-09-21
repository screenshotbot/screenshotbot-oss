;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/batch-promoter
  (:use #:cl)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:check-title
                #:check-status
                #:check-user
                #:check
                #:make-check
                #:push-remote-check
                #:push-remote-check-via-batching)
  (:import-from #:screenshotbot/model/batch
                #:batch-item-title
                #:batch-item-status
                #:batch
                #:batch-name
                #:batch-commit
                #:batch-item-report
                #:batch-item-run
                #:batch-item-channel
                #:find-batch-item
                #:batch-item
                #:batch-items)
  (:import-from #:screenshotbot/user-api
                #:channel-name
                #:recorder-run-commit
                #:recorder-run-channel)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-company
                #:github-repo)
  (:import-from #:hunchentoot-extensions
                #:make-full-url)
  (:import-from #:screenshotbot/dashboard/batch
                #:batch-item-link
                #:sort-items
                #:batch-handler)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:core/installation/installation
                #:installation-domain)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:util/hash-lock
                #:with-hash-lock-held
                #:hash-lock))
(in-package :screenshotbot/batch-promoter)

(named-readtables:in-readtable markup:syntax)

(defvar *lock* (bt:make-lock))

(defvar *push-lock* (make-instance 'hash-lock))

(auto-restart:with-auto-restart ()
 (defmethod push-remote-check-via-batching (promoter
                                            batch
                                            run
                                            check)
   (with-hash-lock-held (batch *push-lock*)
     (let ((item (or
                  (find-batch-item batch :channel (recorder-run-channel run))
                  (make-instance 'batch-item
                                 :channel (recorder-run-channel run)
                                 :batch batch))))
       (setf (batch-item-channel item) (recorder-run-channel run))
       (setf (batch-item-run item) run)
       (setf (batch-item-report item) (report check))
       (setf (batch-item-status item) (check-status check))
       (setf (batch-item-title item) (check-title check))

       (unless (check-status check)
         (warn "Got a NIL status for ~a" run))

       (push-remote-check
        promoter
        batch
        (compute-check batch
                       :user (check-user check)))))))

(defun compute-status (item)
  (let ((statuses (fset:image #'batch-item-status item)))
    (loop for status in (list :rejected
                              :failure
                              :action-required
                              :pending
                              :accepted
                              :success)
          if (fset:contains? statuses status)
            return status)))

(defun compute-title (items)
  (cond
    ((= 1 (fset:size items))
     (batch-item-title (fset:least items)))
    (t
     (ecase (compute-status items)
       (:rejected "Some screenshots were rejected")
       (:failure "Failures")
       (:action-required "Some screenshots need review")
       (:pending "Waiting for a previous run")
       (:accepted "All screenshots accepted")
       (:success "No screenshots changed")))))

(defmethod compute-check ((batch batch)
                          &key user)
  (make-instance 'check
                 :sha (batch-commit batch)
                 :key (batch-name batch)
                 :user user ;; The user who initiated this check request, for audit-logs
                 :title (compute-title (batch-items batch))
                 :details-url (quri:render-uri
                               (quri:merge-uris
                                (hex:make-url
                                 'batch-handler
                                 :oid (oid batch))
                                (installation-domain (installation))))
                 :status (compute-status (batch-items batch))
                 :summary
                 (if (gk:check :markdown-summary t)
                     (build-check-summary batch)
                     "Please review the changes to make sure they look reasonable")))

(defun build-check-summary (batch)
  (markup:write-html
   (let ((items (sort-items (fset:convert 'list (batch-items batch)))))
     <table>
     ,@ (loop for item in items collect
              <tr>
                <td>
                  ,(ecase (batch-item-status item)
                     (:accepted ":white_check_mark:")
                     (:rejected ":x:")
                     (:success ":white_check_mark:")
                     (:failure ":x:")
                     (:pending ":eyes:")
                     (:action-required ":x:"))
                </td>
                <td>
                  <a href= (quri:render-uri (quri:merge-uris
                     (batch-item-link item)
                     (installation-domain (installation))))
                     >,(channel-name (batch-item-channel item))</a>
                </td>
                <td>
                  ,(batch-item-title item)
                </td>
              </tr>)
     </table>)))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/audit-log
  (:use #:cl
        #:screenshotbot/audit-log)
  (:import-from #:screenshotbot/user-api
                #:current-company
                #:created-at)
  (:import-from #:screenshotbot/taskie
                #:timeago)
  (:import-from #:markup
                #:deftag)
  (:import-from #:screenshotbot/dashboard/paginated
                #:paginated)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:render-audit-logs
   #:commit-tag))
(in-package :screenshotbot/dashboard/audit-log)

(markup:enable-reader)

(defmethod render-audit-log ((self base-audit-log))
  (let ((timeago (timeago :timestamp (created-at self))))
    (cond
      ((audit-log-error self)
       <span class= "text-danger">
         ,(describe-audit-log self): <b>error</b>:
       ,(audit-log-error self), tried ,(progn timeago)
       </span>)
      (t
       <span class= "text-success">
         ,(describe-audit-log self) at ,(progn timeago)
       </span>))))

(defmethod describe-audit-log ((self base-audit-log))
  (str:downcase
   (str:replace-all
    "-AUDIT-LOG" ""
    (string (type-of self)))))

(deftag render-audit-logs (&key type (company (current-company))
                           subtitle)
  (let ((audit-logs (audit-logs-for-company company type)))
    <div class= "card mt-3 pb-0">
      <div class= "card-header">
        <h5>API Audit Logs</h5>
      </div>

      <div class= "card-body pb-0">
        <p class= "text-muted">,(progn subtitle)</p>
        <ul class= "mb-0" >
          ,(paginated
            (lambda (audit-log)
              <li>,(render-audit-log audit-log)</li>)
            :items audit-logs)
        </ul>
      </div>
    </div>))

(markup:deftag commit-tag (children)
  (let ((commit (markup:write-html (car children))))
    <code title=commit >,(str:shorten 8 commit)</code>))

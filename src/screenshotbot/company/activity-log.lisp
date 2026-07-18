;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/company/activity-log
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/settings-api
                #:defsettings
                #:settings-template)
  (:import-from #:screenshotbot/dashboard/audit-log
                #:render-audit-log
                #:render-audit-logs)
  (:import-from #:screenshotbot/audit-log
                #:activity-log)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:screenshotbot/login/common
                #:with-login)
  (:import-from #:util/timeago
                #:timeago)
  (:import-from #:screenshotbot/user-api
                #:created-at))
(in-package :screenshotbot/company/activity-log)

(named-readtables:in-readtable markup:syntax)

(defun activity-log-page ()
  (with-login ()
    (cond
      ((not (roles:has-role-p (auth:current-company)
                              (auth:current-user)
                              'roles:admin))
       <simple-card-page>
         <div class= "card-header" >
           <h3>Activity log</h3>
         </div>
         <div class= "alert alert-danger">
           You must be an admin to view this page
         </div>
       </simple-card-page>)
      (t
       <settings-template>
         ,(render-audit-logs
           :title "Activity Logs"
           :type 'activity-log
           :company (auth:current-company))
       </settings-template>))))

(defsettings company-activity-log
  :name "activity-log"
  :section :organization
  :title "Activity Log"
  :handler (lambda ()
             (activity-log-page)))

(defmethod render-audit-log :around ((self activity-log))
  <span>
    ,(call-next-method)
    <span> at <timeago timestamp= (created-at self) />
    </span>
  </span>)

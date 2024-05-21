;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/company/request
  (:use #:cl)
  (:import-from #:core/installation/auth
                #:company-for-request)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature)
  (:import-from #:screenshotbot/user-api
                #:created-at
                #:can-view)
  (:import-from #:screenshotbot/model/user
                #:user-personal-company)
  (:import-from #:local-time
                #:timestamp>
                #:timestamp-)
  (:import-from #:screenshotbot/model/recorder-run
                #:runs-for-company))
(in-package :screenshotbot/company/request)

(defmethod company-for-request ((installation multi-org-feature) request)
  (cond
    ((not (auth:request-user request))
     nil)
    (t
     (guess-best-company
      (auth:session-value :company)
      (auth:request-user request)))))

(defun guess-best-company (company user)
  (when user
   (if (and company (can-view company user))
       company
       (let ((companies (remove-if-not
                         (alexandria:rcurry #'can-view user)
                         (roles:companies-for-user user))))
        (or
         (most-recent-company companies)
         (user-personal-company user)
         (car companies))))))

(defun most-recent-company (companies)
  "Returns the most recently updated company in the list. If none are
  updated in the last month, then return the personal company"
  (cdar
   (sort
    (let ((cutoff (timestamp- (local-time:now) 60 :day)))
      (loop for company in companies
            for run = (fset:greatest (runs-for-company company))
            for created-at = (when run (created-at run))
            if (and created-at (timestamp> created-at cutoff))
              collect
              (cons created-at company)))
    #'timestamp>
      :key #'car)))

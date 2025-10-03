;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/microsoft-teams/audit-log
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:screenshotbot/audit-log
                #:base-audit-log
                #:audit-log-error
                #:audit-logs-for-company)
  (:import-from #:screenshotbot/user-api
                #:%created-at)
  (:export
   #:teams-audit-log
   #:post-to-workflow-audit-log
   #:workflow-name
   #:teams-audit-logs-for-company))
(in-package :screenshotbot/microsoft-teams/audit-log)

(defclass teams-audit-log (base-audit-log)
  ()
  (:metaclass persistent-class))

(defclass post-to-workflow-audit-log (teams-audit-log)
  ((workflow-name :initarg :workflow-name
                  :reader workflow-name)
   (text :initarg :text
         :reader audit-log-text))
  (:metaclass persistent-class))

(defun teams-audit-logs-for-company (company)
  (audit-logs-for-company company 'teams-audit-log))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/gitlab/audit-logs
  (:use #:cl)
  (:import-from #:screenshotbot/audit-log
                #:base-audit-log)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/dashboard/audit-log
                #:commit-tag
                #:describe-audit-log)
  (:import-from #:screenshotbot/user-api
                #:user-full-name)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:update-status-for-user-audit-log
   #:config-updated-audit-log
   #:gitlab-audit-log
   #:check-personal-access-token))
(in-package :screenshotbot/gitlab/audit-logs)

(markup:enable-reader)

(with-class-validation
  (defclass gitlab-audit-log (base-audit-log)
    ()
    (:metaclass persistent-class)))

(with-class-validation
  (defclass update-status-audit-log (gitlab-audit-log)
    ((commit :initarg :commit
             :reader %commit))
    (:metaclass persistent-class)))

(with-class-validation
  (defclass check-personal-access-token (gitlab-audit-log)
    ()
    (:metaclass persistent-class)))

(defmethod describe-audit-log ((self update-status-audit-log))
  <span>
    Updated build status on commit <commit-tag>,(%commit self)</commit-tag>
  </span>)

(with-class-validation
  (defclass config-updated-audit-log (gitlab-audit-log)
    ((%user :initarg :user
            :reader %user))
    (:metaclass persistent-class)))

(defmethod describe-audit-log ((self config-updated-audit-log))
  <span>
    Configuration updated by ,(user-full-name (%user self))
  </span>)

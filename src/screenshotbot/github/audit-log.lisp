;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/audit-log
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:util/misc
                #:?.
                #:uniq)
  (:import-from #:screenshotbot/audit-log
                #:base-audit-log)
  (:import-from #:screenshotbot/dashboard/audit-log
                #:commit-tag
                #:describe-audit-log)
  (:import-from #:screenshotbot/user-api
                #:user-full-name)
  (:import-from #:screenshotbot/model/core
                #:ensure-slot-boundp)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:updated-check-run
   #:user-updated-check-run
   #:check-collaborator
   #:updated-check-run-check))
(in-package :screenshotbot/github/audit-log)

(markup:enable-reader)

(defclass github-audit-log (base-audit-log)
  ((%company :initarg :company
             :index-type hash-index
             :index-reader %github-audit-logs-for-company)
   (err :initarg :error
        :initform nil
        :accessor audit-log-error)
   (error-response :initform nil
                   :accessor audit-log-error-response)
   (http-result-code :initform nil
                     :accessor http-result-code)
   (ts :initarg :ts
       :reader %created-at))
  (:default-initargs :ts (get-universal-time))
  (:metaclass persistent-class))

(defclass updated-check-run (github-audit-log)
  ((commit :initarg :commit
           :reader commit)
   (check :transient t
          :accessor updated-check-run-check
          :documentation "This is only used for debugging old checks"))
  (:metaclass persistent-class))

(defmethod describe-audit-log ((self updated-check-run))
  <span>
    Updated check run on commit <commit-tag>,(commit self)</commit-tag>
  </span>)

(defclass user-updated-check-run (github-audit-log)
  ((%%user :initarg :user
           :reader %user)
   (commit :initarg :commit
           :reader commit)
   (check :transient t
          :accessor updated-check-run-check
          :documentation "This is only used for debugging old checks"))
  (:metaclass persistent-class))

(defmethod describe-audit-log ((self user-updated-check-run))
  <span>
    Updated check run on commit <commit-tag>,(commit self)</commit-tag>
    for review by ,(user-full-name (%user self))
  </span>)

(defclass user-oauth-api-request (github-audit-log)
  ((%%user :initarg :user
           :initform nil
           :reader %user))
  (:metaclass persistent-class))

(defmethod describe-audit-log :around ((self user-oauth-api-request))
  <span>
    ,(call-next-method)
    (using <em>,(?. user-full-name (%user self))</em>'s temporary token)
  </span>)

(defclass check-collaborator (github-audit-log)
  ((github-login :initarg :login
                 :initform nil
                 :reader github-login)
   (repo :initarg :repo
         :initform nil
         :reader github-repo))
  (:metaclass persistent-class))

(defmethod describe-audit-log ((self check-collaborator))
  <span>
    Checked if <code>,(github-login self)</code> is a collaborator on <code>,(github-repo self)</code>
  </span>)

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
                #:uniq)
  (:import-from #:screenshotbot/audit-log
                #:base-audit-log)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:updated-check-run
   #:user-updated-check-run))
(in-package :screenshotbot/github/audit-log)

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
  ()
  (:metaclass persistent-class))

(defclass user-updated-check-run (github-audit-log)
  ((%%user :initarg :user))
  (:metaclass persistent-class))

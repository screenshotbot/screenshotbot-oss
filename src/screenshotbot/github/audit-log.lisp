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
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/github/audit-log)

(defclass github-audit-log (store-object)
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

(defun github-audit-logs-for-company (company)
  (let ((elems (%github-audit-logs-for-company company)))
    (uniq (sort elems #'> :key 'bknr.datastore:store-object-id))))

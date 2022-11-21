;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/pro/bitbucket/audit-log
  (:nicknames :screenshotbot/bitbucket/audit-log)
  (:use #:cl)
  (:import-from #:bknr.indices
                #:skip-list-index)
  (:import-from #:screenshotbot/user-api
                #:%created-at)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/pro/bitbucket/core
                #:bitbucket-error)
  (:import-from #:util/misc
                #:uniq)
  (:import-from #:screenshotbot/model/auto-cleanup
                #:register-auto-cleanup)
  (:import-from #:util/store
                #:with-class-validation)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:audit-log
   #:build-status-audit-log
   #:bitbucket-audit-logs-for-company
   #:audit-log-error-response
   #:build-status-audit-log-commit
   #:build-status-audit-log-full-name
   #:http-result-code
   #:access-token-audit-log
   #:access-token-audit-log-grant-type))
(in-package :screenshotbot/bitbucket/audit-log)

(with-class-validation
 (defclass audit-log (store-object)
   ((%company :initarg :company
              :index-type hash-index
              :index-reader %bitbucket-audit-logs-for-company)
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
   (:metaclass persistent-class)))

(register-auto-cleanup 'audit-log :timestamp #'%created-at)

(with-class-validation
 (defclass build-status-audit-log (audit-log)
   ((commit :initarg :commit
            :reader build-status-audit-log-commit
            :initform nil)
    (full-name :initarg :full-name
               :reader build-status-audit-log-full-name
               :initform nil))
   (:metaclass persistent-class)))

(with-class-validation
 (defclass access-token-audit-log (audit-log)
   ((grant-type :initarg :grant-type
                :reader access-token-audit-log-grant-type))
   (:metaclass persistent-class)))

(defun bitbucket-audit-logs-for-company (company)
  (let ((elems (%bitbucket-audit-logs-for-company company)))
    (uniq (sort elems #'> :key 'bknr.datastore:store-object-id))))

(defun parse-error-response (response result-code audit-log)
  (let* ((response-obj (json:decode-json-from-string response))
         (errors (a:assoc-value response-obj :errors)))
    (let ((message (or
                    (a:assoc-value (car errors) :message)

                    ;; See test-parses-error-correctly
                    (when (equal "error" (a:assoc-value response-obj :type))
                      (a:assoc-value (a:assoc-value response-obj :error)
                                     :message))

                    ;; If we can't parse an actual response, just use the
                    ;; whole json. Suitable for OAuth.
                    response)))
      (warn "Bitbucket api failed with: ~a" message)
      (with-transaction ()
        (setf (http-result-code audit-log) result-code)
        (setf (audit-log-error audit-log) message)
        (setf (audit-log-error-response audit-log) response))))
  (error 'bitbucket-error :audit-log audit-log))

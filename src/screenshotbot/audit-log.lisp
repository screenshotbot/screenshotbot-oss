;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/audit-log
  (:use #:cl)
  (:import-from #:screenshotbot/user-api
                #:%created-at)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/model/auto-cleanup
                #:register-auto-cleanup)
  (:import-from #:util/misc
                #:uniq)
  (:import-from #:screenshotbot/model/core
                #:ensure-slot-boundp)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:util/store/fset-index
                #:fset-set-index)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:base-audit-log
   #:audit-logs-for-company
   #:audit-log-error
   #:with-audit-log))
(in-package :screenshotbot/audit-log)

(with-class-validation
  (defclass base-audit-log (store-object)
    ((%%company :initarg :company
                :index-type fset-set-index
                :index-reader %audit-logs-for-company)
     (%%err :initarg :error
            :initform nil
            :accessor audit-log-error)
     (%%ts :initarg :ts
           :reader %created-at))
    (:default-initargs :ts (get-universal-time))
    (:metaclass persistent-class)))

(register-auto-cleanup 'base-audit-log :timestamp #'%created-at)

(defmethod audit-logs-for-company (company type)
  (reverse
   (fset:convert 'list
                 (fset:filter
                  (lambda (log)
                    (typep log type))
                  (%audit-logs-for-company company)))))

(def-easy-macro with-audit-log (&binding audit-log expr &fn fn)
  (handler-bind ((error
                   (lambda (e)
                     (unless (audit-log-error expr)
                       (with-transaction ()
                         (setf (audit-log-error expr)
                               (format nil "~a" e)))))))
    (funcall fn expr)))

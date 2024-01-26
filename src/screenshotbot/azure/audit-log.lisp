;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/azure/audit-log
  (:use #:cl)
  (:import-from #:screenshotbot/audit-log
                #:base-audit-log)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:util/store/store
                #:with-class-validation)
  (:export
   #:pr-update-request))
(in-package :screenshotbot/azure/audit-log)

(with-class-validation
  (defclass audit-log (base-audit-log)
    ()
    (:metaclass persistent-class)))

(defclass pr-update-request (audit-log)
  ((pr-id :initarg :pr-id
          :reader pr-id)
   (repository-id :initarg :repository-id
                  :reader audit-log-repository-id))
  (:metaclass persistent-class))

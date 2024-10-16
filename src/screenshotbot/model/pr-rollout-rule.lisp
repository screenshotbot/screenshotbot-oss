;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/pr-rollout-rule
  (:use #:cl)
  (:import-from #:util/store/store
                #:with-class-validation
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-unique-index)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-author
                #:recorder-run-work-branch)
  (:import-from #:alexandria
                #:when-let)
  (:export
   #:disable-pull-request-checks-p))
(in-package :screenshotbot/model/pr-rollout-rule)

(defindex +company-index+
  'fset-unique-index
  :slot-name '%company)

(with-class-validation
 (defclass pr-rollout-rule (store-object)
   ((%company :initarg :company
              :index +company-index+
              :index-reader pr-rollout-rule-for-company))
   (:metaclass persistent-class)))

(with-class-validation
  (defclass whitelist-rule (pr-rollout-rule)
    ((emails :initarg :emails
             :initform nil
             :accessor whitelist-rule-emails))
    (:metaclass persistent-class)))

(defmethod disable-pull-request-checks-p (self
                                          run)
  nil)

(defmethod disable-pull-request-checks-p ((self whitelist-rule)
                                          run)
  (not
   (or
    (str:containsp "screenshotbot" (recorder-run-work-branch run)
                   :ignore-case t)
    (when-let ((author (recorder-run-author run)))
     (str:s-member (whitelist-rule-emails self)
                   author)))))


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

(defmethod disable-pull-request-checks-p (self
                                          run)
  nil)


;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/scim/model
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-set-index))
(in-package :screenshotbot/scim/model)

(defindex +config-company-index+
  'fset-set-index
  :slot-name '%company)

(defclass scim-config (store-object)
  ((%company :initarg :company
             :index +config-company-index+
             :index-reader scim-configs-for-company
             :reader scim-config-company)
   (%token :initarg :token
          :reader scim-config-token))
  (:metaclass persistent-class))

(defclass scim-user (store-object)
  ((%company :initarg :company)
   (%emails :initarg :emails
           :initform nil
           :reader scim-user-emails)
   (%activep :initarg :activep
            :initform t
            :reader scim-user-active-p))
  (:metaclass persistent-class))











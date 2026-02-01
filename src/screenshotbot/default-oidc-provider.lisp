;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/default-oidc-provider
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class))
(in-package :screenshotbot/default-oidc-provider)

(defclass default-oidc-provider (store-object)
  ((%scope :initarg :scope
          :reader scope)
   (%client-id :initarg :client-id
               :reader client-id)
   (%client-secret :initarg :client-secret
                   :reader client-secret)
   (%expiration :initarg :expiration-seconds
                :initform (* 20 3600)
                :reader expiration)
   (%issuer :initarg :issuer
            :reader issuer)
   (%company :initarg :company))
  (:metaclass persistent-class))


;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/default-oidc-provider
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:screenshotbot/login/oidc
                #:oidc-provider)
  (:export
   #:make-oidc-provider))
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
   (%company :initarg :company
             :reader company))
  (:metaclass persistent-class))

(defun make-oidc-provider (stored-provider)
  "Create an oidc-provider from a default-oidc-provider instance."
  (make-instance 'oidc-provider
                 :issuer (issuer stored-provider)
                 :client-id (client-id stored-provider)
                 :client-secret (client-secret stored-provider)
                 :scope (scope stored-provider)
                 :identifier 'default-oidc-provider
                 :company-provider (lambda (&rest args)
                                     (declare (ignore args))
                                     (company stored-provider))
                 :expiration-seconds (expiration stored-provider)))


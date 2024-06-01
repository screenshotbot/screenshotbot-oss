;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/model/company-sso
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object))
(in-package :auth/model/company-sso)

(defclass company-sso (store-object)
  ((%company :initarg :company
             :reader company-sso-company)
   (%domain :initarg :domain
            :reader company-sso-domain))
  (:metaclass persistent-class))

(defclass company-oidc-sso (company-sso)
  ((issuer :initarg :issuer
           :accessor company-sso-issuer)
   (client-id :initarg :client-id
              :accessor company-sso-client-id)
   (client-secret :initarg :client-secret
                  :accessor compqany-sso-client-secret))
  (:metaclass persistent-class))

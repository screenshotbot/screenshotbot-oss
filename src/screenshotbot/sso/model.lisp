;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sso/model
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:util/store/store
                #:with-class-validation)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:fake-sso-auth-provider))
(in-package :screenshotbot/sso/model)

(defclass abstract-sso-auth-provider (store-object)
  ()
  (:metaclass persistent-class))

(with-class-validation
  (defclass fake-sso-auth-provider (abstract-sso-auth-provider)
    ()
    (:metaclass persistent-class)))

(with-class-validation
  (defclass basic-sso-auth-provider (abstract-sso-auth-provider)
    ((issuer :initarg :issuer
             :accessor auth-provider-issuer)
     (client-id :initarg :client-id
                :accessor auth-provider-client-id)
     (client-secret :initarg :client-secret
                    :accessor auth-provider-client-secret))
    (:metaclass persistent-class)))

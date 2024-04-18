;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/rpc/rpc
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:screenshotbot/model/api-key
                #:generate-api-secret)
  (:import-from #:util/store/store
                #:with-class-validation)
  (:export
   #:authenticate-rpc-request))
(in-package :core/rpc/rpc)

(defclass rpc-acceptor-mixin ()
  ())

(with-class-validation
  (defclass rpc-auth-id (store-object)
    ((%secret :initarg :secret
              :reader secret)
     (%created-at :initarg :created-at))
    (:metaclass persistent-class)
    (:default-initargs :secret (generate-api-secret)
                      :created-at (get-universal-time))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor rpc-acceptor-mixin)
                                                  request)
  (cond
    ((equal "/intern/rpc" (hunchentoot:script-name request))
     (perform-rpc request))
    (t
     (call-next-method))))

(define-condition rpc-authentication-failed (error)
  ())

(defun authenticate-rpc-request (request)
  (multiple-value-bind (key secret) (hunchentoot:authorization request)
    (let ((key (parse-integer key :junk-allowed t)))
      (unless key
        (error 'rpc-authentication-failed))
      (let ((auth-id (bknr.datastore:store-object-with-id key)))
        (unless (typep auth-id 'rpc-auth-id)
          (error 'rpc-authentication-failed))
        (unless (equal secret (secret auth-id))
          (error 'rpc-authentication-failed))))))

(defmethod perform-rpc (request)
  (authenticate-rpc-request request))

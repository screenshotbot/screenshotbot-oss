;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/default-oidc-provider
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:store-object
                #:persistent-class)
  (:import-from #:screenshotbot/login/oidc
                #:oidc-provider)
  (:import-from #:util/store/object-id
                #:object-with-oid)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:fset-unique-index)
  (:export
   #:default-oidc-provider
   #:oidc-providers-for-company
   #:issuer
   #:make-oidc-provider
   #:active-default-oidc-provider
   #:activep
   #:activate-default-oidc-provider
   #:deactivate-default-oidc-provider))
(in-package :screenshotbot/default-oidc-provider)

(defvar *lock* (bt:make-lock))

(defindex +active-index+
  'fset-unique-index
  :slot-name '%activep)

(defindex +company-index+
  'fset-set-index
  :slot-name '%company)

(defclass default-oidc-provider (object-with-oid)
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
             :index +company-index+
             :index-reader oidc-providers-for-company
             :reader company)
   (%activep :initarg :activep
             :initform nil
             :index +active-index+
             :index-reader %find-by-activep
             :reader activep
             :writer (setf %activep)
             :documentation "Whether this is the login for the installation. At most one
provider can be active, see ACTIVATE-DEFAULT-OIDC-PROVIDER."))
  (:metaclass persistent-class))

(defun active-default-oidc-provider ()
  (%find-by-activep t))

(defun activate-default-oidc-provider (provider)
  "Make PROVIDER the login for this installation, replacing the currently
active provider if there is one."
  (bt:with-lock-held (*lock*)
    (let ((old (active-default-oidc-provider)))
      (with-transaction ()
        ;; Clear the old one first, %ACTIVEP has a unique index.
        (when old
          (setf (%activep old) nil))
        (setf (%activep provider) t)))))

(defun deactivate-default-oidc-provider (provider)
  (with-transaction ()
    (setf (%activep provider) nil)))

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

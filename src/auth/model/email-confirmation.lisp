;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/model/email-confirmation
  (:use #:cl)
  (:import-from #:util/store/object-id
                #:object-with-oid)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:util/store/fset-index
                #:fset-set-index)
  (:export
   #:secret-code
   #:confirmation-code-email
   #:confirmation-confirmed-p
   #:confirmation-user
   #:user-email-confirmed-p
   #:confirm-email))
(in-package :auth/model/email-confirmation)

(defindex +user-index+
  'fset-set-index
  :slot-name '%user)

(with-class-validation
  (defclass email-confirmation-code (object-with-oid)
    ((code :initform (util:make-secret-code)
           :type string
           :reader secret-code)
     (email :type (or null string)
            :initform nil
            :initarg :email
            :reader confirmation-code-email)
     (confirmed-p
      :type boolean
      :initform nil
      :accessor confirmation-confirmed-p)
     (%user
      :initarg :user
      :index +user-index+
      :index-reader %find-by-user
      :accessor confirmation-user))
    (:metaclass persistent-class)))

(defmethod confirm-email ((self email-confirmation-code))
  (setf (confirmation-confirmed-p self) t))

(defmethod user-email-confirmed-p (user)
  (fset:do-set (cc (%find-by-user user))
    (when (confirmation-confirmed-p cc)
      (return t))))

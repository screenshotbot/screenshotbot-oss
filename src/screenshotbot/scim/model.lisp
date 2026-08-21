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
                #:fset-unique-index
                #:fset-set-index)
  (:import-from #:util/store/object-id
                #:oid
                #:object-with-oid)
  (:import-from #:screenshotbot/user-api
                #:user)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:auth
                #:user-email)
  (:import-from #:core/api/model/api-key
                #:generate-api-secret))
(in-package :screenshotbot/scim/model)

(defindex +config-company-index+
  'fset-set-index
  :slot-name '%company)

(defindex +token-index+
  'fset-unique-index
  :slot-name '%token)

(defclass scim-config (store-object)
  ((%company :initarg :company
             :index +config-company-index+
             :index-reader scim-configs-for-company
             :reader scim-config-company)
   (%token :initarg :token
           :index +token-index+
           :index-reader scim-config-for-token
           :reader scim-config-token))
  (:metaclass persistent-class)
  (:default-initargs :token (generate-api-secret)))

(defindex +user-company-index+
  'fset-set-index
  :slot-name '%company)

(defclass scim-user-v2 ()
  ((%company :initarg :company
             :accessor scim-user-company)
   (%user :initarg :user
          :accessor scim-user-user)
   (%external-id :initarg :external-id
                 :initform nil
                 :accessor scim-user-external-id)
   (%emails :initarg :emails
            :initform nil
            :accessor scim-user-emails)
   (user-name :initarg :user-name
              :initform nil
              :accessor scim-user-user-name
              :documentation "This is a unique, transparent, 'ID' on the IdP side. It might be a
username, it might be an email.")
   (%activep :initarg :activep
             :accessor scim-user-activep)
   (fake :initarg :fake
         :initform (error "don't call make-instance directly on scim-user")))
  (:default-initargs :activep t))

(defmethod oid ((self scim-user-v2) &key stringp)
  (declare (ignore stringp))
  (oid (scim-user-user self)))


(defmethod initialize-instance :after ((self scim-user-v2) &key user company
                                                          user-name
                                                          activep)
  (unless user
    ;; We're creating a new user
    (let ((user (make-user
                 :email user-name)))
      (roles:ensure-has-role company user 'roles:disabled-user) 
      (setf (scim-user-user self) user)
      (setf (scim-user-activep self) activep))))


(defmethod scim-users-for-company (company)
  (let ((users (roles:users-for-company company)))
    (loop for user in users
          unless (roles:has-role-p
                  company
                  user 'roles:hidden-user)
          collect
          (user-to-dto
           (make-instance 'scim-user-v2
                          :fake :disregard
                          :emails (list
                                   (user-email user))
                          :company company
                          :user-name (user-email user)
                          :activep (roles:has-role-p company user 'roles:standard-member)
                          :user user)))))

(defmethod (setf scim-user-activep) :after (value scim-user)
  (let ((company (scim-user-company scim-user))
        (user (scim-user-user scim-user)))
    (set-user-activep company user value)))

(defun set-user-activep (company user value)
  (cond
    (value
     (roles:ensure-has-role company user 'roles:standard-member))
    (t
     (setf (roles:user-role company user) 'roles:disabled-user))))


(defun make-scim-user (&rest args)
  (user-to-dto
   (apply #'make-instance 'scim-user-v2
          :fake :disregard
          args)))

(defgeneric user-to-dto (scim-user))



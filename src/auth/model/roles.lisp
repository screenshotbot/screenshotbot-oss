;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/model/roles
  (:use #:cl)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-unique-index
                #:fset-set-index)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:bknr.indices
                #:index-get)
  (:import-from #:alexandria
                #:when-let)
  (:export
   #:set-role
   #:user-role
   #:has-role-p))
(in-package :auth/model/roles)

;;;; See https://phabricator.tdrhq.com/w/user_roles/

(defclass role ()
  ())

(defclass read-only (role)
  ())

(defclass reviewer (role)
  ())

(defclass disabled-user (role)
  ())

(defclass standard-member (read-only reviewer)
  ())

(defclass integrations-developers (standard-member)
  ()
  (:documentation "Can access settings related to creating integrations"))

(defclass admin (standard-member)
  ())

(defclass owner (admin)
  ())

(defclass guest (read-only)
  ())

(defclass external-member (guest reviewer)
  ())

(defclass site-admin (owner)
  ())

(defindex +user-role-index+
  'fset-unique-index
  :slots '(%user %company))

(defclass user-roles (store-object)
  ((%user :initarg :user
          :accessor role-user)
   (%company :initarg :company
             :accessor role-company)
   (role :initarg :role
         :accessor role-type
         :documentation "A symbol corresponding to the type of role."))
  (:class-indices (user-role-index
                   :index +user-role-index+
                   :slots (%user %company)))
  (:metaclass persistent-class))

(defvar *lock* (bt:make-lock))

(defmethod (setf user-role) ((value symbol) company user)
  (bt:with-lock-held (*lock*)
    (let ((role (index-get +user-role-index+ (list user company))))
      (cond
        (role
         (setf (role-type role) value))
        (t
         (make-instance 'user-roles
                        :user user
                        :company company
                        :role value))))))

(defmethod user-role (company user)
  (when-let ((role (index-get +user-role-index+
                              (list user company))))
    (make-instance
     (role-type role))))

(defmethod has-role-p (company user type)
  (assert (find-class type))
  (typep (user-role company user) type))

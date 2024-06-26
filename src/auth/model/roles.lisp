;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/model/roles
  (:use #:cl)
  (:nicknames #:roles)
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
   #:has-role-p
   #:read-only
   #:reviewer
   #:disabled-user
   #:standard-member
   #:integrations-developer
   #:owner
   #:guest
   #:external-member
   #:site-admin
   #:admin
   #:users-for-company
   #:companies-for-user
   #:hidden-user
   #:hidden-admin
   #:role-friendly-name
   #:company-owner
   #:ensure-has-role))
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

(defclass integrations-developer (standard-member)
  ()
  (:documentation "Can access settings related to creating integrations"))

(defclass admin (standard-member)
  ())

(defclass owner (admin)
  ())

(defclass hidden-user (role)
  ()
  (:documentation "Will not show up on the members page"))

(defclass hidden-admin (admin hidden-user)
  ()
  (:documentation "Typically manually set for enterprise installs for site-admins"))

(defclass guest (read-only)
  ())

(defclass external-member (guest reviewer)
  ())

(defclass site-admin (owner)
  ())

(defmethod role-friendly-name ((role role))
  (cond
    ((eql (type-of role) 'standard-member)
     "Member")
    (t
     (str:sentence-case (format nil "~a" (type-of role))))))

(defindex +user-role-index+
  'fset-unique-index
  :slots '(%user %company))

(defindex +company-index+
  'fset-set-index
  :slot-name '%company)

(defindex +user-index+
  'fset-set-index
  :slot-name '%user)

(defclass user-roles (store-object)
  ((%user :initarg :user
          :index +user-index+
          :index-reader user-roles-for-user
          :accessor role-user)
   (%company :initarg :company
             :index +company-index+
             :index-reader user-roles-for-company
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
  (cond
    ((null user) ;; This might happen in scripts
     (warn "Not setting a role for a NIL user for company ~a" company))
    (t
     (bt:with-lock-held (*lock*)
       (let ((role (index-get +user-role-index+ (list user company))))
         (cond
           ((and role (not value))
            ;; delete the role
            (bknr.datastore:delete-object role))
           (role
            (setf (role-type role) value))
           (value
            (make-instance 'user-roles
                           :user user
                           :company company
                           :role value))))))))

(defmethod user-role (company user)
  (when-let ((role (index-get +user-role-index+
                              (list user company))))
    (make-instance
     (role-type role))))

(defmethod users-for-company (company)
  (fset:convert
   'list
   (fset:image #'role-user (user-roles-for-company company))))

(defmethod find-users-for-role (company role)
  (let ((user-roles (user-roles-for-company company)))
    )
  )

(defmethod company-owner (company)
  (fset:do-set (user-role (user-roles-for-company company))
    (when (eql 'owner (role-type user-role))
      (return (role-user user-role)))))

(defmethod companies-for-user (user)
  (fset:convert
   'list
   (fset:image #'role-company (user-roles-for-user user))))

(defmethod has-role-p (company user type)
  "Check if a user has a role of type TYPE. It is allowed to use T as a
type to check if the user is part of the company at all."
  (assert (find-class type))
  (when-let ((role (user-role company user)))
    (typep role type)))

(defmethod ensure-has-role (company user type)
  "Ensures that the user has at least this role. If the role is not
satisfied, then it is set to this role."
  ;; user-role may be overriden, but we want to sync it if that's the
  ;; case.
  (let ((role (user-role company user))
        (saved-role (bknr.indices:index-get +user-role-index+
                                            (list company user))))
    (cond
      ((not (typep role type))
       (setf (user-role company user) type))
      ((not saved-role)
       (setf (user-role company user)
             (type-of role))))))

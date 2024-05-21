;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/model/invite
  (:use :cl)
  (:nicknames :screenshotbot/model/invite)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util
                #:make-secret-code)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:util/store/fset-index
                #:fset-set-index)
  (:export
   #:all-invites
   #:email-count
   #:invite
   #:invite-code
   #:invite-company
   #:invite-email
   #:invite-used-p
   #:invite-with-code
   #:inviter
   #:invites-with-email
   #:all-unused-invites))
(in-package :screenshotbot/model/invite)

(defindex +email-index+
  'fset-set-index
  :slot-name 'email)

(with-class-validation
  (defclass invite (store-object)
    ((code :initform (make-secret-code)
           :accessor invite-code)
     (inviter :initarg :inviter
              :accessor inviter)
     (email :initarg :email
            :initform nil
            :index +email-index+
            :index-reader %invites-with-email
            :reader invite-email)
     (used-p :initform nil
             :accessor invite-used-p)
     (email-count
      :initform 0
      :accessor email-count)
     (company :initarg :company
              :accessor invite-company))
    (:metaclass persistent-class)))

(defindex +proof-user-index+
  'fset-set-index
  :slot-name '%user)

(with-class-validation
  (defclass invite-code-proven (store-object)
    ((%user :initarg :user
            :index +proof-user-index+
            :reader invite-user)
     (%invite :initarg :invite
              :reader invite))
    (:metaclass persistent-class)
    (:documentation "This user has verified that they have access to the invitation
code (probably by clicking the sign-up link from an invitation.")))

(defmethod print-object ((invite invite) out)
  (with-slots (email) invite
   (format out "#<INVITE ~a>" email)))

(defmethod initialize-instance :after ((invite invite) &key company &allow-other-keys)
  (assert company))

(defun all-invites (&key company)
  (loop for invite in (bknr.datastore:class-instances 'invite)
        if (or
            (not company)
            (eql company (invite-company invite)))
          collect invite))

(defun all-unused-invites (&key company)
  (remove-if #'invite-used-p (all-invites :company company)))

(defun invites-with-email (email &key company)
  (let ((invites (fset:convert 'list (%invites-with-email email))))
    (cond
      (company
       (loop for invite in invites
             if (eql (invite-company invite) company)
               collect invite))
      (t
       invites))))

(defun invite-with-code (code &key company)
  (loop for invite in (all-invites :company company)
        if (string= code (invite-code invite))
          return invite))

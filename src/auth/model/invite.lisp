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
                #:with-class-validation)
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
   #:invites-with-email))
(in-package :screenshotbot/model/invite)

(with-class-validation
  (defclass invite (store-object)
    ((code :initform (make-secret-code)
           :accessor invite-code)
     (inviter :initarg :inviter
              :accessor inviter)
     (email :initarg :email
            :initform nil
            :reader invite-email)
     (used-p :initform nil
             :accessor invite-used-p)
     (email-count
      :initform 0
      :accessor email-count)
     (company :initarg :company
              :accessor invite-company))
    (:metaclass persistent-class)))

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

(defun invites-with-email (email &key company)
  (loop for invite in (all-invites :company company)
        if (string= email (invite-email invite))
          collect invite))

(defun invite-with-code (code &key company)
  (loop for invite in (all-invites :company company)
        if (string= code (invite-code invite))
          return invite))

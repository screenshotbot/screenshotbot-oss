;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/invite
  (:use #:cl
        #:alexandria
        #:screenshotbot/notice-api)
  (:import-from #:screenshotbot/model/company
                #:company-invites
                #:all-companies)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util #:make-secret-code)
  (:export
   #:invite
   #:all-invites
   #:invites-with-email
   #:invite-with-code
   #:invite-company
   #:invite-email
   #:inviter
   #:invite-code
   #:email-count))
(in-package :screenshotbot/model/invite)

(defclass invite (store-object)
  ((code :initform (make-secret-code)
         :accessor invite-code)
   (inviter :initarg :inviter
            :accessor inviter)
   (email :initarg :email
          :initform nil
          :reader invite-email)
   (email-count
    :initform 0
    :accessor email-count)
   (company :initarg :company
            :accessor invite-company))
  (:metaclass persistent-class))

(defmethod print-object ((invite invite) out)
  (with-slots (email) invite
   (format out "#<INVITE ~a>" email)))

(defmethod initialize-instance :after ((invite invite) &key company &allow-other-keys)
  (assert company)
  (push invite (company-invites company)))

(defun all-invites (&key company)
  (cond
    (company
     (company-invites company))
    (t
     (loop for company in (all-companies)
           appending (company-invites company)))))

(defun invites-with-email (email &key company)
  (loop for invite in (all-invites :company company)
        if (string= email (invite-email invite))
          collect invite))

(defun invite-with-code (code &key company)
  (loop for invite in (all-invites :company company)
        if (string= code (invite-code invite))
          return invite))

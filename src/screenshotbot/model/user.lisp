;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/user
    (:use #:cl
          #:alexandria
          #:../user-api
          #:../notice-api
          #:../api-key-api)
  (:import-from #:bknr.datastore
                #:store-object-id
                #:with-transaction
                #:persistent-class)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:util
                #:make-secret-code
                #:object-with-oid)
  (:import-from #:./company
                #:company)
  (:import-from #:auth
                #:password-hash)
  ;; classes
  (:export #:user
           #:email-confirmation-code
           #:user-notice)
  ;; Variables
  (:export #:*current-api-key*)
  ;;methods
  (:export #:arnold
           #:user-with-email
           #:user-full-name
           #:user-image-url
           #:github-user
           #:secret-code
           #:professionalp
           #:oauth-user-avatar
           #:user-first-name
           #:oauth-users
           #:confirmation-confirmed-p
           #:user-personal-company
           #:oauth-user-full-name
           #:user-notices
           #:personalp
           #:adminp
           #:unaccepted-invites
           #:user-companies
           #:with-user-lock
           #:user-email
           #:finish-confirmation)
  ;; slots, only for migrations
  (:export #:companies
           #:confirmed-p
           #:email
           #:email-confirmations
           #:password-hash
           #:notices))

(defvar *current-api-key*)

(defun arnold ()
  (user-with-email "arnold@tdrhq.com"))

(defclass user (util:object-with-oid)
  ((full-name :type string
              :initarg :full-name
              :initform nil
              :reader %user-full-name
              :writer (setf user-full-name))
   (email :type (or null string)
          :initarg :email
          :initform nil
          :reader user-email
          :index-initargs (:test #'equal)
          :index-type unique-index
          :index-reader user-with-email
          :index-values all-users
          :writer (setf user-email))
   (password-hash :type (or null string)
                  :initform nil
                  :accessor auth:password-hash)
   (confirmed-p :type boolean
                :accessor confirmed-p)
   (professionalp
    :type boolean
    :initarg :professionalp
    :initform nil
    :accessor professionalp)
   (notices
    :initform nil
    :accessor user-notices)
   (unaccepted-invites
    :initform nil
    :accessor unaccepted-invites)
   (lock
    :transient t
    :initform (bt:make-lock))
   (adminp
    :initform nil
    :accessor adminp)
   (email-confirmations
    :initarg :email-confirmations
    :initform nil
    :accessor email-confirmations)
   (oauth-users
    :initform nil
    :accessor oauth-users)
   (companies
    :initform (list (make-instance 'company :personalp t))
    :initarg :companies
    :accessor user-companies))
  (:metaclass persistent-class))

(defclass user-notice (util:object-with-unindexed-oid)
  ((title :initarg :title
          :accessor notice-title)
   (summary :initarg :summary
            :accessor notice-summary))
  (:metaclass persistent-class))

(defmacro with-user-lock ((&optional (user `(current-user))) &body body)
  `(flet ((body () ,@body))
     (with-slots (lock) ,user
       (bt:with-lock-held (lock)
         (body)))))

(defmethod print-object ((user user) out)
  (format out "#<USER ~a>" (user-email user)))

(defmethod github-user ((user user))
  (loop for oauth-user in (oauth-users user)
        if (typep oauth-user 'github-user)
          do (return oauth-user)))

(defmethod model-id ((user user))
  (store-object-id user))

(defmethod user-full-name ((user user))
  (or (%user-full-name user)
      (when (oauth-users user)
        (assert (car (oauth-users user)))
        (oauth-user-full-name (car (oauth-users user))))))

(defun user-personal-company (user)
  (loop for company in (user-companies user)
        if (personalp company)
          do (return company)))


(defmethod user-first-name ((user user))
  (car (str:split " " (user-full-name user))))


(defclass email-confirmation-code (object-with-oid)
  ((code :initform (make-secret-code)
         :type string
         :reader secret-code)
   (email :type (or null string)
          :initform nil
          :initarg :email
          :reader confirmation-code-email)
   (confirmed-p
    :type boolean
    :accessor confirmation-confirmed-p)
   (user
    :initarg :user
    :accessor confirmation-user))
  (:metaclass persistent-class))

(defun finish-confirmation (confirmation)
  (let ((user (confirmation-user confirmation)))
    (with-transaction ()
      (push confirmation (email-confirmations user))
      (setf (confirmation-confirmed-p confirmation) t)
      (setf (confirmed-p user) t))))

(defmethod user-image-url (user &rest args)
  (if (oauth-users user)
      (oauth-user-avatar (car (oauth-users user)))
      (format nil "~a" (apply 'gravatar:image-url
                              (user-email user)
                              args))))

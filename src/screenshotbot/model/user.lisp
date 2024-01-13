;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/user
  (:use #:cl
        #:alexandria
        #:screenshotbot/user-api
        #:screenshotbot/notice-api
        #:screenshotbot/api-key-api)
  (:import-from #:bknr.datastore
                #:store-object-id
                #:with-transaction
                #:persistent-class)
  (:import-from #:screenshotbot/installation
                #:one-owned-company-per-user
                #:multi-org-feature
                #:installation)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:util
                #:make-secret-code
                #:object-with-oid)
  (:import-from #:screenshotbot/model/company
                #:company-owner
                #:get-singleton-company
                #:company)
  (:import-from #:auth
                #:password-hash
                #:oauth-user-avatar
                #:oauth-user-full-name
                #:oauth-user-user
                #:oauth-user-email)
  (:import-from #:bknr.datastore
                #:store-objects-with-class)
  (:import-from #:bknr.indices
                #:destroy-object)
  (:import-from #:util/store
                #:with-class-validation
                #:def-store-local)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/model/github
                #:github-user)
  (:export
   #:user
   #:email-confirmation-code
   #:user-notice)
  (:export
   #:arnold
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
   #:oauth-user-email
   #:oauth-user-user
   #:personalp
   #:adminp
   #:unaccepted-invites
   #:user-companies
   #:with-user-lock
   #:user-email
   #:finish-confirmation)
  (:export
   #:companies
   #:confirmed-p
   #:email
   #:email-confirmations
   #:password-hash
   #:notices
   #:users-for-company
   #:default-company
   #:make-user))
(in-package :screenshotbot/model/user)

(defvar *lock* (bt:make-lock))

(def-store-local *lowercase-email-map*
    (make-hash-table :test #'equal)
  "A map from lowercase emails to the user. There might be stale
  mappings, so please use user-with-email to access this map, and
  don't directly use this map.")

(defun arnold ()
  (user-with-email "arnold@tdrhq.com"))

(with-class-validation
  (defclass user (util:object-with-oid)
    ((full-name :type (or null string)
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
            :index-reader %user-with-email
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
      :accessor auth:oauth-users)
     (companies
      :initarg :companies
      :initform nil
      :accessor %user-companies
      :documentation "This companies slot is only use in a multi-org
    set-up. A default installation of Screenshotbot OSS, would be a
    single org set up.")
     (default-company
      :initarg :default-company
      :initform nil
      :writer (setf default-company)
      :reader %default-company
      :documentation "The default company when this user logs in. We'll
    change this when the user 'switches' companies in the UI. If no
    default company is provided it should default to the personal
    company."))
    (:metaclass persistent-class)))

(define-condition user-email-exists (error)
  ((email :initarg :email)))

(defmethod print-object ((e user-email-exists) out)
  (format out "The user already exists with email: ~a"
          (slot-value e 'email)))

(defmethod update-lowercase-email-map ((user user))
  "Update the lowercase email index. This might be called inside the transaction to make-user, in which case it should correctly error and not update the state"
  (when (slot-boundp user 'email)
    (let ((email (str:downcase (user-email user))))
      (when email ;; mostly for tests
        (symbol-macrolet ((place (gethash email *lowercase-email-map*)))
          (let ((prev-user place))
            (cond
              ((null prev-user)
               (setf place user))
              ((not (eql prev-user user))
               (error 'user-email-exists :email (user-email user))))))))))

(defmethod bknr.datastore:initialize-transient-instance :after ((user user))
  (update-lowercase-email-map user))

;; (mapc #'update-lowercase-email-map (all-users))

(defun all-users ()
  (store-objects-with-class 'user))

(defun user-with-email (email)
  (let ((old-val (%user-with-email email)))
   (let ((user
           (gethash (str:downcase email) *lowercase-email-map*)))
     (cond
       ((and user
             (string-equal email (user-email user)))
        user)
       (t
        (when old-val
          ;; Safety measure. Once we verify this is not happening, we
          ;; can safely delete the use of old-val and just always
          ;; return nil
          (warn "The new index did not match the new-index for ~a" email))
        old-val)))))

(defun make-user (&rest args &key companies &allow-other-keys)
  (let ((user (apply #'make-instance 'user args)))
    (unless companies
      (initialize-companies-for-user user (installation)))
    (push-event :user.created)
    user))

(defmethod initialize-companies-for-user (user installation)
  (values))

(defmethod initialize-companies-for-user (user (installation multi-org-feature))
  (with-transaction ()
    (setf (%user-companies user)
          (list
           (make-instance 'company
                           :personalp t
                           :admins (list user)
                           :owner user)))))

(defmethod initialize-companies-for-user (user (installation
                                                one-owned-company-per-user))
  (values))

(defmethod destroy-object :before ((user user))
  (loop for company in (user-companies user)
        do
           (when (eql user (company-owner company))
             (setf (company-owner company) nil))))

(defmethod users-for-company ((company company))
  ;; this is inefficient, but.. it gets the job done and isn't called
  ;; very often.
  (loop for user in (all-users)
        if (member company (user-companies user))
          collect user))

(defmethod user-companies ((user user))
  (user-companies-for-installation user (installation)))

(defmethod (setf user-companies) (companies (user user))
  (setf (user-companies-for-installation user (installation))
        companies))

(defmethod user-companies-for-installation ((user user) (installation multi-org-feature))
  (%user-companies user))

(defmethod (setf user-companies-for-installation) (companies (user user) (installation multi-org-feature))
  (setf (%user-companies user) companies))


(defmethod user-companies-for-installation ((user user) installation)
  (list
   (get-singleton-company installation)))

(defmethod (setf user-companies-for-installation) (companies (user user) installation)
  (declare (ignore companies user installation))
  (error "Can't set user-companies with multi-org-feature"))

(with-class-validation
  (defclass user-notice (util:object-with-unindexed-oid)
    ((title :initarg :title
            :accessor notice-title)
     (summary :initarg :summary
              :accessor notice-summary))
    (:metaclass persistent-class)))

(defmacro with-user-lock ((&optional (user `(current-user))) &body body)
  `(flet ((body () ,@body))
     (with-slots (lock) ,user
       (bt:with-lock-held (lock)
         (body)))))

(defmethod print-object ((user user) out)
  (format out "#<USER ~a>" (user-email user)))

(defmethod user-full-name ((user user))
  (or (%user-full-name user)
      (when (auth:oauth-users user)
        (assert (car (auth:oauth-users user)))
        (oauth-user-full-name (car (auth:oauth-users user))))))

(defun user-personal-company (user)
  (loop for company in (user-companies user)
        if (personalp company)
          do (return company)))


(defmethod user-first-name ((user user))
  (car (str:split " " (user-full-name user))))


(with-class-validation
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
    (:metaclass persistent-class)))

(defun finish-confirmation (confirmation)
  (let ((user (confirmation-user confirmation)))
    (with-transaction ()
      (push confirmation (email-confirmations user))
      (setf (confirmation-confirmed-p confirmation) t)
      (setf (confirmed-p user) t))))

(defmethod user-image-url (user &rest args)
  (let ((known-avatar
          (?. oauth-user-avatar (car (auth:oauth-users user)))))
    (cond
      ((str:emptyp known-avatar)
       (format nil "~a" (apply 'gravatar:image-url
                               (user-email user)
                               args)))
      (t
       known-avatar))))

(defmethod default-company ((user user))
  (let ((user-companies (user-companies  user)))
   (or
    (find (%default-company user) user-companies)
    (user-personal-company user)
    (car user-companies))))

(defmethod (setf user-email) :after (email (user user))
  (update-lowercase-email-map user))

(defmethod auth:find-user ((self installation) &key email)
  (user-with-email email))

(defmethod auth:find-or-create-user ((self installation) &key email)
  (bt:with-lock-held (*lock*)
   (or
    (values (user-with-email email) nil)
    (values (make-user :email email) t))))

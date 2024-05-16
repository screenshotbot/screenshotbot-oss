;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/user
  (:use :cl)
  (:import-from #:auth
                #:current-user
                #:oauth-user-avatar
                #:oauth-user-email
                #:oauth-user-full-name
                #:user-first-name
                #:oauth-user-user
                #:password-hash
                #:user-email)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-objects-with-class
                #:with-transaction)
  (:import-from #:bknr.indices
                #:destroy-object
                #:unique-index)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature
                #:one-owned-company-per-user)
  (:import-from #:screenshotbot/login/github
                #:github-user)
  (:import-from #:screenshotbot/model/company
                #:company-admin-p
                #:company
                #:company-owner
                #:get-singleton-company)
  (:import-from #:screenshotbot/notice-api
                #:notice-summary
                #:notice-title)
  (:import-from #:screenshotbot/user-api
                #:adminp
                #:all-users
                #:personalp
                #:unaccepted-invites
                #:user
                #:user-companies
                #:user-full-name
                #:user-image-url
                #:user-notices)
  (:import-from #:util
                #:make-secret-code)
  (:import-from #:util/events
                #:push-event)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:util/store/object-id
                #:object-with-oid)
  (:import-from #:util/store/store
                #:def-store-local
                #:with-class-validation)
  (:import-from #:auth/model/email-confirmation
                #:user-email-confirmed-p
                #:secret-code
                #:finish-confirmation)
  (:import-from #:auth/model/roles
                #:owner
                #:admin
                #:user-role
                #:standard-member)
  (:local-nicknames (#:roles #:auth/model/roles))
  (:export
   #:adminp
   #:arnold
   #:companies
   #:confirmation-confirmed-p
   #:confirmed-p
   #:default-company
   #:email
   #:email-confirmation-code
   #:email-confirmations
   #:finish-confirmation
   #:github-user
   #:make-user
   #:notices
   #:oauth-user-avatar
   #:oauth-user-email
   #:oauth-user-full-name
   #:oauth-user-user
   #:oauth-users
   #:password-hash
   #:personalp
   #:professionalp
   #:secret-code
   #:unaccepted-invites
   #:user
   #:user-companies
   #:user-email
   #:user-first-name
   #:user-full-name
   #:user-image-url
   #:user-notice
   #:user-notices
   #:user-personal-company
   #:user-with-email
   #:users-for-company
   #:with-user-lock))
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
                  :writer (setf confirmed-p)
                  :reader %confirmed-p
                  :documentation "Don't think we're actually reading this from anywhere. Look at user-email-confirmed-p instead.")
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
      :accessor unaccepted-invites
      :accessor auth:unaccepted-invites)
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
  "TODO: 5/13/24: delete")

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

(define-condition cant-set-user-companies (error)
  ()
  (:report "Can't set user-companies without multi-org-feature"))

(defmethod (setf user-companies-for-installation) (companies (user user) installation)
  (declare (ignore companies user installation))
  (error 'cant-set-user-companies))

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

(defmethod finish-confirmation ((confirmation email-confirmation-code))
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

(defmethod auth:make-user ((self installation) &rest args &key &allow-other-keys)
  (apply #'make-user args))

(defmethod user-email-confirmed-p ((user user))
  (or
   (call-next-method)
   (%confirmed-p user)))

(defmethod user-role ((company company) (user user))
  "To support transitioning to the new role based access control, we
override user-role."
  (or
   (call-next-method)
   (when (member company (user-companies user))
     (make-instance 'standard-member))))

(defmethod (setf roles:user-role) :after (value (company company) (user user))
  (when value
    (handler-case
        (pushnew company (user-companies user))
      (cant-set-user-companies (e)
        ;;ignored
        (values)))))

(defmethod (setf roles:user-role) :after ((value null) (company company) (user user))
  (handler-case
      (alexandria:removef (user-companies user)  company)
    (cant-set-user-companies (e)
      ;; ignored
      (values))))

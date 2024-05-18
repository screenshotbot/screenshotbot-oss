;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/company
  (:use #:cl
        #:alexandria
        #:screenshotbot/user-api
        #:screenshotbot/screenshot-api)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:unique-index
                #:with-transaction
                #:store-object)
  (:import-from #:screenshotbot/task-integration-api
                #:enabledp)
  (:import-from #:util
                #:find-by-oid
                #:object-with-oid)
  (:import-from #:bknr.datastore
                #:deftransaction)
  (:import-from #:screenshotbot/installation
                #:oss-installation
                #:multi-org-feature
                #:installation)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:fset-unique-index)
  (:import-from #:core/installation/auth
                #:company-for-request)
  (:import-from #:core/installation/auth-provider
                #:company-sso-auth-provider)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:import-from #:screenshotbot/model/core
                #:ensure-slot-boundp)
  (:import-from #:core/installation/installation
                #:installation-domain
                #:*installation*)
  (:import-from #:auth/login/roles-auth-provider
                #:get-company-for-auth-provider)
  (:export
   #:company
   #:company-reports
   #:github-config
   #:access-token
   #:company-channels
   #:jira-config-url
   #:jira-config-username
   #:slack-config-channel
   #:jira-config-password
   #:enabledp
   #:company
   #:find-image-by-id
   #:company-with-name
   #:find-or-create-channel
   #:verified-p
   #:jira-config-project-id
   #:github-config
   #:installation-id
   #:company-admins
   #:jira-config
   #:phabricator-config
   #:slack-config
   #:phabricator-config-for-company
   #:company-with-singletonp
   #:singletonp
   #:phabricator-url
   #:conduit-api-key
   #:default-slack-config
   #:jira-config
   #:add-company-run
   #:company-owner
   #:add-company-report
   #:maybe-redirect-for-company)
  (:local-nicknames (#:roles #:auth/model/roles)))
(in-package :screenshotbot/model/company)

(defindex +singleton-index+
  'fset-unique-index
  :slot-name 'singletonp)

(with-class-validation
  (defclass company (object-with-oid)
    ((name
      :initarg :name
      :accessor %company-name
      :writer (setf company-name)
      :index-initargs (:test 'equal)
      :index-type unique-index
      :index-reader company-with-name
      :index-values all-named-companies)
     (personalp
      :initarg :personalp
      :accessor personalp
      :initform nil)
     (owner
      :reader %company-owner
      :initform :roles
      :documentation "It used to be the actual company owner, but if it's :roles, then
 the owner will be pulled from user-roles.")
     (admins
      :initarg :admins
      :initform nil
      :reader company-admins)
     (runs
      :initarg :runs
      :initform nil
      :accessor company-runs)
     (reports
      :initarg :reports
      :initform nil
      :accessor company-reports)
     (invites
      :initform nil
      :documentation "DEPRECATED")
     (channels
      :initarg :channels
      :initform nil
      :accessor company-channels)
     (default-slack-config
      :initform nil
      :initarg :default-slack-config
      :accessor default-slack-config)
     (github-config
      :initform nil)
     (jira-config
      :initform nil
      :reader %jira-config
      :writer (setf jira-config))
     (demo-filled-p
      :initform nil)
     (singletonp
      :initarg :singletonp
      :index +singleton-index+
      :index-reader company-with-singletonp)
     (sso-auth-provider
      :initform nil
      :accessor company-sso-auth-provider)
     (images
      :initarg :company-images
      :initform nil
      :documentation "deprecated list of images. do not use.")
     (%redirect-url
      :initarg :redirect-url
      :accessor redirect-url
      :documentation "Redirect this company to this URL. This is used if we want to migrate a company to a different domain.")
     (invitation-role
      :initarg :invitation-role
      :accessor company-invitation-role))
    (:metaclass persistent-class)
    (:default-initargs :redirect-url nil
                       :invitation-role 'roles:standard-member)))

(defmethod initialize-instance :after ((self company) &rest args &key owner)
  (when (and owner (not (eql :roles owner)))
    (roles:ensure-has-role self owner 'roles:owner)))

(defindex +parent-index+
  'fset-set-index
  :slot-name 'parent)

(with-class-validation
  (defclass sub-company (company)
    ((parent :initarg :parent
             :initform nil
             :index +parent-index+
             :index-reader sub-companies-of
             :reader company-parent))
    (:metaclass persistent-class)
    (:documentation "A child organization, by default has all the permissions of the
parent organization.")))

(defmethod print-object ((self company) out)
  (format out "#<COMPANY ~a>" (ignore-errors (company-name self))))

(let ((lock (bt:make-lock "jira-config")))
  (defmethod jira-config ((company company))
    "For historical reasons, our company links to the jira config. We'll get rid of this in a future version"
    (bt:with-lock-held (lock)
      (or
       (%jira-config company)
       (with-transaction ()
        (setf
         (jira-config company)
         (make-instance 'jira-config :enabledp nil)))))))

(defmethod singletonp ((company company))
  (slot-boundp company 'singletonp))

(with-class-validation
 (defclass github-config (store-object)
   ((installation-id
     :initform nil
     :accessor installation-id))
   (:metaclass persistent-class)))

(with-class-validation
  (defclass phabricator-config (store-object)
    ((company :initarg :company
              :index-initargs (:test 'eql)
              :index-type unique-index
              :index-reader %phabricator-config-for-company)
     (url :initarg :url
          :initform nil
          :accessor phabricator-url)
     (api-key :initarg :api-key
              :initform nil
              :accessor conduit-api-key))
    (:metaclass persistent-class)))

(let ((lock (bt:make-lock)))
 (defun phabricator-config-for-company (company)
   (bt:with-lock-held (lock)
     (or
      (%phabricator-config-for-company company)
      (make-instance 'phabricator-config :company company)))))


(defmethod github-config ((company company))
  (with-slots (github-config) company
   (or
    github-config
    (with-transaction ()
      (or
       github-config
       (setf github-config (make-instance 'github-config)))))))

(with-class-validation
  (defclass slack-config (store-object)
    ((access-token
      :initarg :access-token
      :initform nil
      :relaxed-object-reference t
      :accessor access-token)
     (channel
      :initarg :channel
      :accessor slack-config-channel)
     (enabledp
      :initarg :enabledp
      :initform nil
      :accessor enabledp))
    (:metaclass persistent-class)))

(with-class-validation
  (defclass jira-config (store-object)
    ((url :initarg :url
          :initform nil
          :accessor jira-config-url)
     (username :initarg :username
               :initform nil
               :accessor jira-config-username)
     (password :initarg :password
               :initform nil
               :accessor jira-config-password)
     (project-id :initarg :project-id
                 :initform nil
                 :accessor jira-config-project-id)
     (enabledp
      :initarg :enabledp
      :initform nil
      :accessor enabledp))
    (:metaclass persistent-class)))

(defmethod (setf company-admins) (val (company company))
  ;; always have at least one admin
  (assert (> (length val) 0))
  (setf (slot-value company 'admins) val))


(defun company-name (company)
  (cond
    ((personalp company)
     ;; there should be just one admin
     (let ((name (anaphora:awhen (car (company-admins company))
                   (user-full-name anaphora:it))))
       (cond
         ((str:emptyp name)
          "Default")
         (t
          name))))
    ((and
      (singletonp company)
      (or
       (not (slot-boundp company 'name))
       (not (slot-value company 'name))))
     ;; TODO: we probably don't need this. We could probably fix this
     ;; in a migration in the future to make sure that every singleton
     ;; company has a name.
     "Singleton Company")
    (t
     (let ((name (slot-value company 'name)))
       name))))


(defmethod find-image ((company company) hash)
  (error "Old function: call model/image:find-image instead"))

(defgeneric find-image-by-id (company id))

(defmethod find-or-create-channel ((company company) name)
  (or
   (loop for channel in (company-channels company)
         if (equal (channel-name channel) name)
           return channel)
   (let ((channel (make-instance 'channel :name name :company company)))
     (with-transaction ()
      (pushnew channel (company-channels company)))
     channel)))

(defmethod can-view ((company company) user)
  (and
   (roles:has-role-p company user 'roles:read-only)
   (company-domain-matches-installation-p company)))

(defun company-domain-matches-installation-p (company)
  (let ((redirect-url (redirect-url company)))
    (cond
      ((null redirect-url)
       t)
      (t
       (equal
        (installation-domain *installation*)
        redirect-url)))))

(defmethod can-view ((company sub-company) user)
  (or
   (call-next-method)
   (can-view (company-parent company) user)))

(defgeneric company (obj)
  (:documentation "For a given obj, figure out which company it belongs to"))

(defmethod prepare-singleton-company-for-installation ((installation installation))
  "Get a singleton persistent company. If no singleton company exists,
  it is created. Otherwise the existing singleton company is
  returned. Singleton companies are mostly used in the OSS version,
  even though you can customize it to use multiple companies."
  (unless (company-with-singletonp t)
    (make-instance 'company :singletonp t)))

(defmethod prepare-singleton-company-for-installation ((installation multi-org-feature))
  "We never create a singleton company for multi-orgs"
  (values))

(defun prepare-singleton-company ()
  (when-let (installation (ignore-errors (installation)))
   (prepare-singleton-company-for-installation installation)))

(defmethod get-singleton-company ((installation installation))
  (company-with-singletonp t))

(defmethod get-singleton-company ((installation multi-org-feature))
  (error "singleton company doesn't make sense in a multi-org mode"))

(util:add-datastore-hook 'prepare-singleton-company)

(deftransaction
    add-company-run (company run)
    (check-type company company)
    (check-type run store-object)
    (push run (company-runs company)))

(defmethod company-admin-p ((company company) user)
  (roles:has-role-p company user 'roles:admin))

(deftransaction
    add-company-report (company report)
    (check-type company company)
    (check-type report store-object)
    (push report (company-reports company)))

(defmethod company-for-request ((installation installation) request)
  (get-singleton-company installation))

(def-store-migration ("Add :redirect-url to companies" :version 10)
  (ensure-slot-boundp 'company '%redirect-url))


(defun maybe-redirect-for-company (company)
  "When called within a hunchentoot requests, redirects to the redirect
URL for the company, if there is one."
  (when-let  ((redirect-url (redirect-url company)))
    (hunchentoot:redirect
     (quri:render-uri
      (quri:merge-uris
       (quri:uri (hunchentoot:request-uri*))
       redirect-url)))))

(def-store-migration ("Add default :invitation-role" :version 12)
  (ensure-slot-boundp 'company 'invitation-role :value 'roles:standard-member))

(defun ensure-company-using-roles (company)
  "This is only used for a migration"
  (when-let ((owner (slot-value company 'owner)))
    (unless (eql :roles owner)
      (setf (slot-value company 'owner) :roles)
      (roles:ensure-has-role company owner 'roles:owner))))

(def-store-migration ("Ensure company owner uses roles" :version 13)
  (ensure-slot-boundp 'company 'owner)
  (mapc #'ensure-company-using-roles
        (bknr.datastore:class-instances 'company)))

(defmethod company-owner ((self company))
  (let ((owner (%company-owner self)))
    (cond
      ((eql :roles owner)
       (roles:company-owner self))
      (t
       (warn "Using old company-owner schema")
       owner))))

(def-store-migration ("Ensure company-admins is using roles" :version 14)
  (ensure-slot-boundp 'company 'admins)
  (dolist (company (bknr.datastore:class-instances 'company))
    (dolist (user (slot-value company 'admins))
      (roles:ensure-has-role company user 'roles:admin))))

(def-store-migration ("Clear company-admins slot" :version 15)
  ;; We accidentally forgot to do this in the previous migration
  (dolist (company (bknr.datastore:class-instances 'company))
    (setf (slot-value company 'admins) nil)))

(defmethod get-company-for-auth-provider ((installation oss-installation)
                                          auth-provider)
  (get-singleton-company installation))

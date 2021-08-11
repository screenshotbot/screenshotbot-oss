;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/company
    (:use #:cl
          #:alexandria
          #:../user-api
          #:../screenshot-api)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:unique-index
                #:with-transaction
                #:store-object)
  ;; temporary hack, remove it: it should not be needed
  (:import-from #:../task-integration-api
                #:enabledp)
  (:import-from #:util
                #:find-by-oid
                #:object-with-oid)
  (:import-from #:bknr.datastore
                #:deftransaction)
  (:export #:company
           #:company-reports
           #:github-config
           #:find-image
           #:access-token
           #:image-cache
           #:company-invites
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
           #:all-companies
           #:slack-config
           #:phabricator-config-for-company
           #:company-with-singletonp
           #:singletonp
           #:phabricator-url
           #:conduit-api-key
           #:default-slack-config
           #:jira-config
           #:add-company-run))

(defclass company (object-with-oid)
  ((name
    :initarg :name
    :accessor %company-name
    :index-initargs (:test 'equal)
    :index-type unique-index
    :index-reader company-with-name
    :index-values all-named-companies)
   (personalp
    :initarg :personalp
    :accessor personalp
    :initform nil)
   (owner
    :initarg :owner
    :accessor company-owner
    :initform nil)
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
    :accessor company-invites)
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
    :index-type unique-index
    :index-reader company-with-singletonp)
   (images
    :initarg :company-images
    :initform nil
    :documentation "deprecated list of images. do not use.")
   (image-cache
    :initform (make-hash-table :test 'equal)
    :accessor image-cache
    :transient t
    :documentation "Cache from image hash to list of all images"))
  (:metaclass persistent-class))

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

(defun migrate-images-to-image-cache (company)
  (with-slots (images) company
    (dolist (image images)
      (with-transaction ()
        (setf (company image) company)))))

(defclass github-config (store-object)
  ((installation-id
    :initform nil
    :accessor installation-id))
  (:metaclass persistent-class))

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
  (:metaclass persistent-class))

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

(defclass slack-config (store-object)
  ((access-token
    :initarg :access-token
    :initform nil
    :accessor access-token)
   (channel
    :initarg :channel
    :accessor slack-config-channel)
   (enabledp
    :initarg :enabledp
    :initform nil
    :accessor enabledp))
  (:metaclass persistent-class))

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
  (:metaclass persistent-class))


;; eh, we could do better
(defun all-companies ()
  (remove-duplicates
   (loop for user in (all-users)
         appending (user-companies user))))


(defmethod (setf company-admins) (val (company company))
  ;; always have at least one admin
  (assert (> (length val) 0))
  (setf (slot-value company 'admins) val))


(defun company-name (company)
  (cond
    ((personalp company)
     ;; there should be just one admin
     (anaphora:awhen (car (company-admins company))
       (user-full-name anaphora:it)))
    ((singletonp company)
     "Singleton Company")
    (t
     (slot-value company 'name))))


(defmethod find-image ((company company) hash)
  (loop for im in (gethash hash (image-cache company))
        if (verified-p im)
          return im))

(defmethod find-image-by-id ((company company) id)
  (let ((obj (find-by-oid id)))
    (typecase obj
      (local-image
       obj)
      (t
       (assert (eql company (company obj)))
       obj))))

(defmethod find-or-create-channel ((company company) name)
  (or
   (loop for channel in (company-channels company)
         if (equal (channel-name channel) name)
           return channel)
   (let ((channel (make-instance 'channel :name name :company company)))
     (with-transaction ()
      (pushnew channel (company-channels company)))
     channel)))

(defmethod find-run-by-id ((company company) id)
  (when (stringp id)
    (setf id (parse-integer id)))
  (loop for run in (company-runs company)
        if (eql id (model-id run))
          return run))

(defmethod can-view ((company company) user)
  (member company (user-companies user)))

(defgeneric company (obj)
  (:documentation "For a given obj, figure out which company it belongs to"))

(let ((lock (bt:make-lock)))
  (defun get-singleton-company ()
    "Get a singleton persistent company. If no singleton company exists,
  it is created. Otherwise the existing singleton company is
  returned."
    (or
     (company-with-singletonp t)
     (bt:with-lock-held (lock)
       (or
        (company-with-singletonp t)
        (make-instance 'company :singletonp t))))))

(deftransaction
    add-company-run (company run)
    (check-type company company)
    (check-type run store-object)
    (push run (company-runs company)))

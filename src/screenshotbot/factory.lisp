;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/factory
    (:use #:cl
          #:alexandria
          #:./template
          #:./user-api
          #:./screenshot-api
          #:./report-api
          #:./git-repo
          #:./model/github
          #:./api-key-api)
  (:import-from #:./github/access-checks
                #:github-repo)
  (:export #:test-user
           #:test-company
           #:test-screenshot
           #:test-recorder-run
           #:test-image
           #:test-channel
           #:test-api-key
           #:*user*
           #:*company*))

(defclass test-user ()
  ((api-keys :initarg :api-keys
             :initform nil)))

(defclass test-company ()
  ((runs :initarg :runs
         :accessor company-runs)))

(defclass test-channel ()
  ((name :initform "dummy-channel"
         :initarg :name
         :accessor channel-name)
   (%created-at :initform 10
                :accessor %created-at)
   (object-id :initform 1
              :accessor bknr.datastore:store-object-id)
   (repo :initarg :repo
         :initform (make-instance 'github-repo :link "https://github.com/tdrhq/foo.git")
         :accessor channel-repo)))

(defclass test-recorder-run ()
  ((commit :initform "foo"
           :initarg :commit
           :accessor recorder-run-commit)))


(defclass test-image ()
  ((url :initarg :url
        :initform "https://foo"
        :accessor image-public-url)))

(defmethod user-api-keys ((user test-user) company)
  (slot-value user 'api-keys))

(defclass test-api-key ()
  ((key :initarg :key
        :accessor api-key-key)
   (secret :initarg :secret
           :accessor api-key-secret-key)))

(defclass test-screenshot ()
  ((name :initarg :name
         :accessor screenshot-name)))

(defvar *user* (make-instance 'test-user))
(defvar *company* (make-instance 'test-company))

(defmethod screenshotbot/model/user:user-image-url ((u test-user) &rest args)
  "https://foo/bar.jpg")

(defmethod screenshotbot/model/user:personalp ((u test-company))
  t)

(defmethod screenshotbot/model/user:user-full-name ((u test-user))
  "Arnold Noronha")

(Defmethod screenshotbot/model/user:user-companies ((u test-user))
  (list
   *company*))

(defmethod screenshotbot/model/user:adminp ((u test-user))
  nil)

(defmethod screenshotbot/model/company:company-admins ((c test-company))
  nil)

(defmethod screenshotbot/model/user:unaccepted-invites ((u test-user))
  nil)

(defmethod screenshotbot/model/user:user-notices ((u test-user))
  nil)

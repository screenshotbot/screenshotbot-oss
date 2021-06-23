;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/github
    (:use #:cl
          #:../user-api
          #:alexandria)
  (:import-from #:bknr.datastore
                #:store-object
                #:unique-index
                #:store-object-id
                #:persistent-class)
  (:import-from #:./user
                #:github-user
                #:oauth-user-full-name
                #:oauth-user-user
                #:oauth-user-avatar)
  ;; classes
  (:export #:oauth-access-token
           #:github-access-token
           #:github-user)
  ;; methods
  (:export #:%find-github-user-by-id
           #:oauth-user-email
           #:oauth-user-full-name
           #:oauth-user-avatar
           #:oauth-user-user
           #:github-login
           #:known-emails
           #:access-token-string))

(defclass oauth-access-token (store-object)
  ((access-token :type (or null string)
                 :initarg :access-token
                 :accessor access-token-string)
   (expires-in :type (or null integer)
               :initarg :expires-in)
   (refresh-token :type (or null string)
                  :initarg :refresh-token)
   (refresh-token-expires-in :type (or null integer)
                             :initarg :refresh-token-expires-in)
   (scope :type (or null string)
          :initarg :scope)
   (token-type :type (or null string)
               :initarg :token-type))
  (:metaclass persistent-class))

;; For old bknr.datastore objects
(defclass github-access-token (oauth-access-token)
  ()
  (:metaclass persistent-class))

(defclass github-user (store-object)
  ((gh-user-id :type integer
               :initarg :gh-user-id
               :index-type unique-index
               :initform nil
               :index-initargs (:test 'equal)
               :index-reader %find-github-user-by-id)
   (email :type (or null string)
          :initarg :email
          :initform nil
          :accessor oauth-user-email)
   (access-token
    :accessor access-token)
   (full-name :type (or null string)
              :initarg :full-name
              :initform nil
              :accessor oauth-user-full-name)
   (avatar :type (or null string)
           :initarg :avatar
           :initform nil
           :accessor oauth-user-avatar)
   (login :initform nil
          :initarg :login
          :accessor github-login)
   (known-emails :initform nil
                 :accessor known-emails)
   (user :initarg :user
         :initform nil
         :accessor oauth-user-user))
  (:metaclass persistent-class))

(defmethod model-id ((inst github-user))
  (store-object-id inst))

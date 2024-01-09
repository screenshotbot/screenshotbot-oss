;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/github
  (:use :cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object
                #:store-object-id)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:screenshotbot/model/user
                #:github-user
                #:oauth-user-avatar
                #:oauth-user-email
                #:oauth-user-full-name
                #:oauth-user-user)
  (:import-from #:screenshotbot/user-api
                #:user)
  (:import-from #:util/store/store
                #:with-class-validation)
  (:export
   #:%find-github-user-by-id
   #:access-token-string
   #:github-access-token
   #:github-login
   #:github-user
   #:known-emails
   #:oauth-access-token
   #:oauth-user-avatar
   #:oauth-user-email
   #:oauth-user-full-name
   #:oauth-user-user))
(in-package :screenshotbot/model/github)

(with-class-validation
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
     (full-name :type (or null string)
                :initarg :full-name
                :initform nil
                :reader %oauth-user-full-name
                :writer (setf oauth-user-full-name))
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
    (:metaclass persistent-class)))

(defmethod oauth-user-full-name ((self github-user))
  (cond
    ((str:emptyp (%oauth-user-full-name self))
     (github-login self))
    (t
     (%oauth-user-full-name self))))

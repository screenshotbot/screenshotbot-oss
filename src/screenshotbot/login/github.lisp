;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/github
  (:use :cl)
  (:nicknames :screenshotbot/model/github)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:auth
                #:oauth-user-avatar
                #:oauth-user-email
                #:oauth-user-full-name
                #:oauth-user-user)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:util/store/permissive-persistent-class
                #:permissive-persistent-class)
  (:import-from #:util/store/store
                #:with-class-validation)
  (:import-from #:util/store/migrations
                #:ensure-symbol-in-package)
  (:export
   #:%find-github-user-by-id
   #:access-token-string
   #:github-access-token
   #:github-login
   #:known-emails
   #:oauth-access-token
   #:oauth-user-avatar
   #:oauth-user-email
   #:oauth-user-full-name
   #:oauth-user-user))
(in-package :screenshotbot/login/github)

(ensure-symbol-in-package
 #:github-user
 :old #:screenshotbot/model/user
 :new #:screenshotbot/model/github)

(export 'github-user)

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
    (:metaclass permissive-persistent-class)))

(defmethod oauth-user-full-name ((self github-user))
  (cond
    ((str:emptyp (%oauth-user-full-name self))
     (github-login self))
    (t
     (%oauth-user-full-name self))))

(defmethod github-user (user)
  "Get the github-user associated with a given user"
  (warn "Calling github-user method, we thought this was unsed")
  (loop for gu in (bknr.datastore:class-instances 'github-user)
        if (eql user (oauth-user-user gu))
          return gu))
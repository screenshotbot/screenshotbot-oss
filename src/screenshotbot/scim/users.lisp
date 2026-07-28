;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/scim/users
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/scim/model
                #:scim-config-company
                #:scim-config-for-token
                #:scim-config-token
                #:scim-configs-for-company)
  (:import-from #:util/misc
                #:not-null!)
  (:import-from #:screenshotbot/scim/dto
                #:list-response
                #:external-user
                #:external-email)
  (:import-from #:auth
                #:user-email)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:util/json-mop
                #:json-mop-to-string))
(in-package :screenshotbot/scim/users)


(defun bearer-token ()
  (let ((header (hunchentoot:header-in* :authorization)))
    (destructuring-bind (type token)
        (str:split " " (str:trim header) :limit 2)
      (unless (string-equal "bearer" type)
        (error "Only bearer token supported"))
      (str:trim token))))

(defun get-company! ()
  (let ((token (bearer-token)))
   (let ((scim-config (not-null! (scim-config-for-token (not-null! token)))))
     (not-null! (scim-config-company scim-config)))))

(defhandler (nil :uri "/scim/v2/Users" :method :get) ()
  (let ((company (get-company!)))
    (let ((users (roles:users-for-company company)))
      (json-mop-to-string
       (make-instance
        'list-response
        :total-results (length users)
        :start-index 1
        :items-per-page (length users)
        :resources
        (loop for user in users
              collect
              (make-instance 'external-user
                             :id (format nil "~a" (bknr.datastore:store-object-id user))
                             :user-name (user-email user)
                             :emails (list (make-instance 'external-email
                                                          :type "work"
                                                          :value (user-email user))))))))))




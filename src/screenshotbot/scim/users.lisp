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
                #:scim-config-token
                #:scim-configs-for-company)
  (:import-from #:util/misc
                #:not-null!))
(in-package :screenshotbot/scim/users)


(defun bearer-token ()
  (let ((header (hunchentoot:header-in* :authorization)))
    (destructuring-bind (type token)
        (str:split " " (str:trim header) :limit 2)
      (unless (string-equal "bearer" type)
        (error "Only bearer token supported"))
      (str:trim token))))

(defun check-token! (company)
  (let ((token (bearer-token)))
   (loop for config in (fset:convert 'list (scim-configs-for-company (not-null! company)))
         if (equal token (scim-config-token config))
           return t
         finally
         (error "Could not validate request"))))

(defhandler (nil :uri "/scim/:oid/Users") (oid)
  (let ((company (util:find-by-oid oid)))
    (check-token! company)
    (error "unimpl")))



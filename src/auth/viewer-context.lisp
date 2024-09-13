;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/viewer-context
  (:use #:cl)
  (:export
   #:viewer-context-user
   #:viewer-context
   #:api-viewer-context
   #:normal-viewer-context
   #:anonymous-viewer-context
   #:email-viewer-context
   #:viewer-context-api-key))
(in-package :auth/viewer-context)

(defclass abstract-viewer-context ()
  ()
  (:documentation "The same viewer might view the object through different contexts. For
example a viewer viewer via API or browser should have different
permissions. Or a super-admin might not want to browse with
super-admin priviledges by default."))

(defclass logged-in-viewer-context (abstract-viewer-context)
  ((user :initarg :user
         :reader viewer-context-user)))

(defclass api-viewer-context (logged-in-viewer-context)
  ((api-key :initarg :api-key
            :reader viewer-context-api-key)))

(defclass normal-viewer-context (logged-in-viewer-context)
  ())

(defclass viewer-context (normal-viewer-context)
  ())

(defclass anonymous-viewer-context (abstract-viewer-context)
  ())

(defclass site-admin-viewer-context (normal-viewer-context)
  ())

(defclass email-viewer-context (logged-in-viewer-context)
  ((user :initarg :user
         :reader viewer-context-user))
  (:documentation "Information being sent over an email. Technically not a 'logged-in'
viewer context, but I suppose the user is logged in to their email
address so it's logged in in that sense."))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/installation/auth
  (:use #:cl)
  (:export
   #:call-with-ensure-user-prepared
   #:company-for-request
   #:find-user))
(in-package :core/installation/auth)

(defgeneric call-with-ensure-user-prepared (installation user
                                            fn)
  (:method (installation user fn)
    (funcall fn))
  (:documentation "A web callback to ensure that a user is prepared before rending a
with-login page."))

(defgeneric company-for-request (installation request))

(defgeneric find-user (installation &key email))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/model
  (:use #:cl
        #:auth))
(in-package :auth/model)

(defgeneric find-or-create-user (installation &key email)
  (:documentation "Finds a user by email, or creates a new user if no such user
exists. Returns two values: the user and a boolean indicating if it
was a new user."))
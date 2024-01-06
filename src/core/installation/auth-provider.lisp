;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/installation/auth-provider
  (:use #:cl)
  (:export
   #:auth-provider
   #:auth-provider-signin-form
   #:auth-provider-signup-form))
(in-package :core/installation/auth-provider)

(defclass auth-provider ()
  ())

(defgeneric auth-provider-signin-form (auth-provider redirect))

(defgeneric auth-provider-signup-form (auth-provider invite-code
                                       plan
                                       redirect))

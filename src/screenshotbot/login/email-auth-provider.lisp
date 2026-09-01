;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/email-auth-provider
  (:use #:cl)
  (:import-from #:screenshotbot/login/common
                #:standard-auth-provider)
  (:export
   #:screenshotbot-email-auth-provider))
(in-package :screenshotbot/login/email-auth-provider)

(defclass screenshotbot-email-auth-provider (standard-auth-provider)
  ()
  (:documentation "Extends some login behavior for the standard email login flow."))


;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/login/saml
  (:use #:cl)
  (:import-from #:screenshotbot/login/login
                #:auth-provider)
  (:import-from #:screenshotbot/login/common
                #:oauth-logo-svg
                #:oauth-signin-link
                #:abstract-oauth-provider))
(in-package :auth/login/saml)

(defclass saml-provider (abstract-oauth-provider)
  ((metadata :initarg :metadata)))

(defmethod oauth-signin-link ((self saml-provider) redirect)
  "/foo")

(defmethod oauth-logo-svg ((self saml-provider))
  nil)

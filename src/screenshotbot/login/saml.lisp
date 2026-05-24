;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/saml
  (:use #:cl)
  (:import-from #:core/installation/auth-provider
                #:auth-provider)
  (:import-from #:auth/login/roles-auth-provider
                #:roles-auth-provider)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:export
   #:saml-auth-provider))
(in-package :screenshotbot/login/saml)

(defclass saml-auth-provider (auth-provider
                              roles-auth-provider)
  ((entity-id :initform "https://screenshotbot.io"
              :initarg :entity-id)
   (idp-metadata-url :initarg :idp-metadata-url)))

(defhandler (nil :uri "/sso/saml/callback") ()
  (error "unimpl"))


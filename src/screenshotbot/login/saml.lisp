;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/saml
  (:use #:cl)
  (:import-from #:core/installation/auth-provider
                #:auth-provider-signin-form
                #:auth-provider)
  (:import-from #:auth/login/roles-auth-provider
                #:roles-auth-provider)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:export
   #:saml-auth-provider))
(in-package :screenshotbot/login/saml)

(named-readtables:in-readtable markup:syntax)

(defclass saml-auth-provider (auth-provider
                              roles-auth-provider)
  ((entity-id :initform "https://screenshotbot.io"
              :initarg :entity-id)
   (idp-metadata-url :initarg :idp-metadata-url)
   (name :initarg :name
         :initform "generic-saml"
         :reader saml-name)))

(defhandler (nil :uri "/sso/saml/callback") ()
  (error "unimpl"))

(defmethod signin-link ((self saml-auth-provider) redirect)
  "#")

(defmethod logo-svg ((self saml-auth-provider))
  nil)

(defmethod auth-provider-signin-form ((self saml-auth-provider) redirect)
  <div class= "form-group mt-1 text-center mb-0">
    <a class= "btn btn-outline-secondary" style= "width:100%"  href= (signin-link self redirect) >
      ,(logo-svg self)
      <span class= "ms-1">Sign in with ,(saml-name self) </span>
    </a>
  </div>)

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/email-auth-provider
  (:use #:cl)
  (:import-from #:screenshotbot/login/common
                #:email-redirect-url
                #:standard-auth-provider)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:screenshotbot/model/enterprise
                #:enterprise-install-domain
                #:email-domains
                #:enterprise-install)
  (:export
   #:screenshotbot-email-auth-provider))
(in-package :screenshotbot/login/email-auth-provider)

(defclass screenshotbot-email-auth-provider (standard-auth-provider)
  ()
  (:documentation "Extends some login behavior for the standard email login flow."))

(defmethod email-redirect-url ((self screenshotbot-email-auth-provider) domain)
  (loop for enterprise in (class-instances 'enterprise-install)
        do
           (loop for enterprise-email-domain in (email-domains enterprise)
                 if (equal domain enterprise-email-domain)
                   do
                      (return-from email-redirect-url
                        (make-login-url (enterprise-install-domain enterprise))))))

(defun make-login-url (domain)
  (quri:render-uri
   (quri:make-uri
    :path "/login"
    :defaults (quri:uri domain))))



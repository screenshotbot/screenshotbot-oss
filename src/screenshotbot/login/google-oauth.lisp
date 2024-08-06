;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/google-oauth
  (:use :cl)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:oidc/oidc
                #:make-oidc-auth-link
                #:verify-userinfo)
  (:import-from #:screenshotbot/login/common
                #:oauth-logo-svg)
  (:import-from #:screenshotbot/login/oidc
                #:identifier
                #:oidc-provider
                #:oidc-user)
  (:import-from #:util/threading
                #:with-extras)
  (:export
   #:google-access-token
   #:google-oauth-provider
   #:google-oauth-redirect
   #:google-user
   #:make-google-oauth-link))
(in-package :screenshotbot/login/google-oauth)

(markup:enable-reader)

(defclass google-oauth-provider (oidc-provider)
  ((domain :initform nil
           :initarg :domain
           :reader google-domain
           :documentation "Restrict this google oauth to this domain"))
  (:default-initargs
   :issuer "https://accounts.google.com"
   :scope  "openid email profile"
   :identifier  'google
   :oauth-name "Google"))

;; datastore backward compatibility
(defclass google-user (oidc-user)
  ((identifier :initform 'google))
  (:metaclass persistent-class))

(defmethod oauth-logo-svg ((auth google-oauth-provider))
  (declare (ignore auth))
  <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-google" viewBox="0 0 16 16">
  <path d="M15.545 6.558a9.42 9.42 0 0 1 .139 1.626c0 2.434-.87 4.492-2.384 5.885h.002C11.978 15.292 10.158 16 8 16A8 8 0 1 1 8 0a7.689 7.689 0 0 1 5.352 2.082l-2.284 2.284A4.347 4.347 0 0 0 8 3.166c-2.087 0-3.86 1.408-4.492 3.304a4.792 4.792 0 0 0 0 3.063h.003c.635 1.893 2.405 3.301 4.492 3.301 1.078 0 2.004-.276 2.722-.764h-.003a3.702 3.702 0 0 0 1.599-2.431H8v-3.08h7.545z"/>
  </svg>)

(defmethod make-oidc-auth-link :around ((self google-oauth-provider)
                                        redirect
                                        &key &allow-other-keys)
  (cond
    ((google-domain self)
     (quri:render-uri
      (let ((old (quri:uri (call-next-method))))
        (quri:copy-uri
         old
         :query (list*
                 `("hd" . ,(google-domain self))
                 (quri:uri-query-params old))))))
    (t
     (call-next-method))))

(defmethod verify-userinfo ((self google-oauth-provider) user-info)
  (when (google-domain self)
    (let ((expected-domain (assoc-value user-info :hd))
          (actual-domain (google-domain self)))
      (with-extras (("expected-domain" expected-domain)
                    ("actual-domain" actual-domain))
       (assert (equal expected-domain
                      actual-domain))))))

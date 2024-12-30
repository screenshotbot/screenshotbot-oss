;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sso/redirect
  (:use #:cl)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:core/installation/auth-provider
                #:company-sso-auth-provider)
  (:import-from #:screenshotbot/login/oidc
                #:oidc-provider)
  (:import-from #:screenshotbot/sso/model
                #:auth-provider-client-secret
                #:auth-provider-client-id
                #:auth-provider-issuer)
  (:import-from #:alexandria
                #:when-let*)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/login/common
                #:oauth-signin-link)
  (:import-from #:auth/login/sso
                #:maybe-redirect-for-sso))
(in-package :screenshotbot/sso/redirect)

(defmethod maybe-redirect-for-sso ((company company))
  ;; 1. check if SSO will actually help
  ;; 2. actually do the redirect.
  (when-let* ((sso-auth-provider (company-sso-auth-provider company))
              (oidc-provider (make-instance 'oidc-provider
                                            :identifier 'self-service
                                            :issuer (auth-provider-issuer sso-auth-provider)
                                            :client-id (auth-provider-client-id sso-auth-provider)
                                            :client-secret (auth-provider-client-secret sso-auth-provider)
                                            :expiration-seconds (* 24 3600))))
    (hex:safe-redirect
     (oauth-signin-link oidc-provider
                        (nibble:allow-user-change
                         (nibble ()
                           (error "don't know what to do")))))))


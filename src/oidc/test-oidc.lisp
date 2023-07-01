;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :oidc/test-oidc
  (:use #:cl
        #:fiveam)
  (:import-from #:oidc/oidc
                #:after-authentication
                #:user-info
                #:oauth-access-token
                #:userinfo-endpoint
                #:oauth-get-access-token
                #:token-endpoint
                #:oidc-callback
                #:oidc)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:cl-mock
                #:if-called
                #:answer))
(in-package :oidc/test-oidc)

(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    (with-fake-request ()
     (auth:with-sessions ()
       (let ((auth (make-instance 'oidc
                                  :client-id "meh-client-id"
                                  :client-secret "meh-client-secret"
                                  :issuer "https://example.com"
                                  :callback-endpoint "/account/oauth-callback")))
         (&body))))))

(test simple-oidc-callback
  (let ((token (make-instance 'oauth-access-token
                              :access-token "my-token")))
   (with-fixture state ()
     (answer (token-endpoint auth) "https://example.com/token")
     (answer (userinfo-endpoint auth) "https://example.com/userinfo")
     (answer (oauth-get-access-token
              "https://example.com/token"
              :CLIENT_ID "meh-client-id"
              :CLIENT_SECRET "meh-client-secret"
              :CODE "code"
              :REDIRECT_URI "http://localhost/account/oauth-callback")
       token)
     (answer (user-info auth token)
       (json:decode-json-from-string "{\"sub\":2,\"email\":\"arnold@tdrhq.com\",\"fullName\":\"Arnold Noronha\",\"avatar\":\"https://imgur.com/foo.png\"}"))
     (answer (after-authentication
              auth
              :user-id 2
              :email "arnold@tdrhq.com"
              :full-name "Arnold Noronha"
              :avatar "https://imgur.com/foo.png")
       nil)
     (catch 'hunchentoot::handler-done
       (oidc-callback
        auth
        "code" "/"))
     (pass))))

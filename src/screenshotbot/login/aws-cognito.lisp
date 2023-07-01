;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/aws-cognito
  (:use #:cl)
  (:import-from #:screenshotbot/login/oidc
                #:oidc-provider)
  (:import-from #:oidc/oidc
                #:client-id
                #:authorization-endpoint
                #:logout-link)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/user-api
                #:current-user)
  (:export
   #:aws-cognito))
(in-package :screenshotbot/login/aws-cognito)

(named-readtables:in-readtable markup:syntax)

(defclass aws-cognito (oidc-provider)
  ())

(defhandler (logout-confirmation :uri "/cognito/logout-confirmation") ()
  (setf (current-user) nil)
  <html>
    <body>
      You are now logged out. <a href= "/">Go back here</a>.
    </body>
  </html>)

(defmethod logout-link ((self aws-cognito))
  "See https://docs.aws.amazon.com/cognito/latest/developerguide/logout-endpoint.html.

In particular this means that /cognito/logout-confirmation must be in your 'Allowed sign-out URLs'"
  (let ((url (authorization-endpoint self)))
    (quri:render-uri
     (quri:copy-uri
      (quri:uri url)
      :path "/logout"
      :query `(("client_id" . ,(client-id self))
               ("logout_uri" . ,(hex:make-full-url
                                   hunchentoot:*request*
                                   'logout-confirmation)))))))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/authentication
  (:use #:cl)
  (:import-from #:screenshotbot/auth-server/model
                #:access-token-resource
                #:oauth-access-token)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:screenshotbot/api/core
                #:api-error-msg
                #:api-error))
(in-package :screenshotbot/auth-server/authentication)

(define-condition resource-mismatch-error (api-error)
  ((resource :initarg :resource)
   (script-name :initarg :script-name)))

(defmethod api-error-msg ((self resource-mismatch-error))
  (with-slots (resource script-name) self
    (format nil "Resource ~a did not match script ~a"
            resource script-name)))

(defmethod %authenticate-request (request (api-key oauth-access-token))
  "Extracted out for testing convenience"
  (when-let ((resource (access-token-resource api-key)))
   (let ((resource (quri:uri resource)))
     (unless (str:starts-with-p (quri:uri-path resource)
                                (hunchentoot:script-name request))
       (error 'resource-mismatch-error
              :resource (quri:uri-path resource)
              :script-name (hunchentoot:script-name request))))))

(defmethod screenshotbot/api/core:authenticate-request-from-key :before ((request auth:authenticated-request)
                                                                         (api-key oauth-access-token))
  (%authenticate-request request api-key))

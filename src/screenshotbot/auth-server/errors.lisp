;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/errors
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:oauth-error
   #:oauth-error-code
   #:oauth-error-description
   #:oauth-error-status
   #:oauth-error!
   #:oauth-error-alist
   #:with-oauth-json-errors
   #:write-json))
(in-package :screenshotbot/auth-server/errors)

(define-condition oauth-error (error)
  ((code :initarg :code
         :reader oauth-error-code
         :documentation "One of the error codes from RFC 6749 §4.1.2.1/§5.2, RFC 8628 §3.5, etc.")
   (description :initarg :description
                :initform nil
                :reader oauth-error-description)
   (status :initarg :status
           :initform 400
           :reader oauth-error-status
           :documentation "The HTTP status to use when this is rendered on the token endpoint."))
  (:report (lambda (self stream)
             (format stream "OAuth error ~a: ~a"
                     (oauth-error-code self)
                     (or (oauth-error-description self) "")))))

(defun oauth-error! (code &optional description &key (status 400))
  (error 'oauth-error
         :code code
         :description description
         :status status))

(defun oauth-error-alist (e)
  "The wire representation of E, as an alist ready for CL-JSON.

RFC 6749 §5.2 requires `error`, and allows `error_description`."
  `(("error" . ,(oauth-error-code e))
    ,@(when (oauth-error-description e)
        `(("error_description" . ,(oauth-error-description e))))))

(defun write-json (alist)
  "Serialize ALIST as a JSON response body, with the headers OAuth requires.

RFC 6749 §5.1 requires responses to be non-cacheable."
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (setf (hunchentoot:header-out :cache-control) "no-store")
  (setf (hunchentoot:header-out :pragma) "no-cache")
  (json:encode-json-alist-to-string alist))

(def-easy-macro with-oauth-json-errors (&fn fn)
  "Run FN, rendering any OAUTH-ERROR as a JSON error response.

This is the error behaviour for the token, device-code and revocation
endpoints. The authorization endpoint is different: it has to redirect
errors back to the client, so it handles OAUTH-ERROR itself."
  (handler-case
      (funcall fn)
    (oauth-error (e)
      (setf (hunchentoot:return-code*) (oauth-error-status e))
      (when (equal "invalid_client" (oauth-error-code e))
        ;; RFC 6749 §5.2: a 401 must carry a challenge.
        (setf (hunchentoot:header-out :www-authenticate) "Basic realm=\"oauth\""))
      (write-json (oauth-error-alist e)))))

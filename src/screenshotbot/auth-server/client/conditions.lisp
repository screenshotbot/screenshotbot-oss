;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/conditions
  (:use #:cl)
  (:export
   #:client-error
   #:oauth-error
   #:oauth-error-code
   #:oauth-error-description
   #:oauth-error!
   #:http-error
   #:http-error-status
   #:http-error-url
   #:http-error-body
   #:authorization-declined
   #:authorization-timeout
   #:state-mismatch))
(in-package :auth-server/client/conditions)

(define-condition client-error (error)
  ()
  (:documentation "Anything that went wrong while signing in."))

(define-condition oauth-error (client-error)
  ((code :initarg :code
         :reader oauth-error-code
         :documentation "The `error` member of an RFC 6749 §5.2 error response.")
   (description :initarg :description
                :initform nil
                :reader oauth-error-description))
  (:report (lambda (self stream)
             (format stream "~a~@[: ~a~]"
                     (oauth-error-code self)
                     (oauth-error-description self)))))

(defun oauth-error! (code &optional description)
  (error 'oauth-error :code code :description description))

(define-condition http-error (client-error)
  ((status :initarg :status :reader http-error-status)
   (url :initarg :url :reader http-error-url)
   (body :initarg :body :initform nil :reader http-error-body))
  (:report (lambda (self stream)
             (format stream "HTTP ~a from ~a~@[~%~a~]"
                     (http-error-status self)
                     (http-error-url self)
                     (http-error-body self)))))

(define-condition authorization-declined (client-error)
  ((description :initarg :description :initform nil))
  (:report (lambda (self stream)
             (declare (ignore self))
             (format stream "You declined the authorization request."))))

(define-condition authorization-timeout (client-error)
  ((seconds :initarg :seconds :initform nil))
  (:report (lambda (self stream)
             (with-slots (seconds) self
               (format stream "Gave up after ~a seconds waiting for authorization."
                       seconds)))))

(define-condition state-mismatch (client-error)
  ()
  (:report (lambda (self stream)
             (declare (ignore self))
             (format stream "The redirect carried the wrong state parameter, so it did ~
not come from the request we started. Ignoring it."))))

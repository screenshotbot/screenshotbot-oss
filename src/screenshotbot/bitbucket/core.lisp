(defpackage :screenshotbot/pro/bitbucket/core
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:bitbucket-error
   #:http-success-response?))
(in-package :screenshotbot/pro/bitbucket/core)

(define-condition bitbucket-error (error)
  ((%audit-log :initarg :audit-log)))

(defun http-success-response? (response-code)
  (<= 200 response-code 204))

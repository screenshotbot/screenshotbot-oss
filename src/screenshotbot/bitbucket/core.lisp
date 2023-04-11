;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/bitbucket/core
  (:nicknames :screenshotbot/pro/bitbucket/core)
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:bitbucket-error
   #:http-success-response?))
(in-package :screenshotbot/bitbucket/core)

(define-condition bitbucket-error (error)
  ((%audit-log :initarg :audit-log)))

(defun http-success-response? (response-code)
  (<= 200 response-code 204))

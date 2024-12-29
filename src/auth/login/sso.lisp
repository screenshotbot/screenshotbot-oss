;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/login/sso
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro))
(in-package :auth/login/sso)

(define-condition needs-sso-condition (condition)
  ((company :initarg :company
            :reader needs-sso-condition-company)))

(defgeneric maybe-redirect-for-sso (company)
  (:documentation "Maybe redirect if SSO is enabled for the company, might not return if redirected."))

(def-easy-macro with-handle-needs-sso (&fn impl)
  (let ((sso-company))
    (handler-bind ((needs-sso-condition
                     (lambda (condition)
                       (setf sso-company (needs-sso-condition-company condition))))
                   (auth:no-access-error
                     (lambda (e)
                       (declare (ignore e))
                       (when sso-company
                         (maybe-redirect-for-sso sso-company)))))
      (funcall impl))))




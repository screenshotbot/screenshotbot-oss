;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/require-invite-sso-mixin
  (:use #:cl)
  (:import-from #:oidc/oidc
                #:authentication-error
                #:after-authentication)
  (:import-from #:auth/model/invite
                #:invite-used-p
                #:invites-with-email)
  (:import-from #:screenshotbot/model/user
                #:user-with-email)
  (:import-from #:screenshotbot/invite
                #:accept-invite)
  (:export
   #:require-invite-sso-mixin))
(in-package :screenshotbot/login/require-invite-sso-mixin)

(defclass require-invite-sso-mixin ()
  ((disallow-domains :initarg :disallow-domains
                     :reader disallow-domains))
  (:documentation "If there's no invite for this SSO, then don't allow it."))

(define-condition disallowed-domain-error (authentication-error)
  ()
  (:default-initargs :message
   "You may not create an account this email domain. Please use SSO instead."))

(define-condition no-invite-error (authentication-error)
  ()
  (:default-initargs :message "You must have an invite to be able to login"))

(defmethod after-authentication :around ((self require-invite-sso-mixin) &key
                                                                           email
                                         &allow-other-keys)
  (let ((user (user-with-email email)))
   (cond
     ((and
       user
       (roles:companies-for-user user))
      (call-next-method))
     ((not (invites-with-email email))
      (error 'no-invite-error))
     (t
      (loop for domain in (disallow-domains self)
            if (str:ends-with-p (format nil "@~a" domain) email)
              do (error 'disallowed-domain-error))
      (prog1
          (call-next-method)
        (accept-all-invites (auth:current-user) (invites-with-email email)))))))

(defun accept-all-invites (user invites)
  (loop for invite in invites do
    (accept-invite invite :redirect nil :user user)
    (setf (invite-used-p invite) t)))

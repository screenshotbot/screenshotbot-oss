;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/login/roles-auth-provider
  (:use #:cl)
  (:import-from #:core/installation/auth-provider
                #:on-user-sign-in)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:export
   #:roles-auth-provider))
(in-package :auth/login/roles-auth-provider)

(defclass roles-auth-provider ()
  ((company-provider :initarg :company-provider
                     :initform nil
                     :reader company-provider
                     :documentation "A function, when evaluated, produces the company for which we should add a default logged-in user."))
  (:documentation "Adds a logged in user to a default role for a company"))


(defmethod on-user-sign-in :after ((self roles-auth-provider)
                                   user)
  (let ((company (get-company-for-auth-provider
                  *installation*
                  self)))
    (when company
      (roles:ensure-has-role company user 'roles:standard-member))))

(defmethod get-company-for-auth-provider (installation (self roles-auth-provider))
  "By creating a method that is specialized on installation, we can make
sure the OSS version behaves a certain way. Technically a little ugly,
but it's a good API."
  (when (company-provider self)
    (funcall (company-provider self))))

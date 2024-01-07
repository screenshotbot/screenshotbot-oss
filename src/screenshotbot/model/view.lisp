;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/view
  (:use :cl)
  (:import-from #:auth
                #:current-user
                #:no-access-error
                #:can-view
                #:can-edit
                #:can-edit!
                #:can-view!)
  (:import-from #:screenshotbot/user-api
                #:adminp
                #:can-public-view
                #:user)
  (:export
   #:can-edit
   #:can-edit!
   #:can-public-view
   #:can-view
   #:can-view!))
(in-package :screenshotbot/model/view)

;; This file adds logic to check if a specific object can be viewed by
;; the given user

(defmethod can-view :around (obj (user user))
  (or
   (call-next-method)
   ;; super-admins can access everything
   (adminp user)))

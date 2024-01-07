;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/view
  (:use :cl)
  (:import-from #:auth
                #:current-user
                #:no-access-error)
  (:import-from #:screenshotbot/user-api
                #:adminp
                #:can-public-view
                #:can-view
                #:can-view!
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

(defgeneric can-view (obj user))

(defgeneric can-public-view (obj))

(defun can-view! (&rest objects)
  (let ((user (current-user)))
    (dolist (obj objects)
      (unless (or
               (can-public-view obj)
               (and user
                    (can-view obj user)))
        (restart-case
            (error 'no-access-error
                    :user user
                    :obj obj)
          (give-access-anyway ()
            nil))))))

(defmethod can-view :around (obj (user user))
  (or
   (call-next-method)
   ;; super-admins can access everything
   (adminp user)))

(defmethod can-edit! (&rest objects)
  (let ((user (current-user)))
    (dolist (obj objects)
      (unless (can-edit obj user)
        (error 'no-access-error :user user :obj obj)))))

(defmethod can-edit (obj user)
  nil)

(defmethod can-edit :around (obj user)
  (and
   user
   (can-view obj user)
   (call-next-method)))

(defmethod can-public-view (obj)
  nil)

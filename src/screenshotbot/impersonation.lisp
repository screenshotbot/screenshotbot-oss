;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/impersonation
  (:use :cl)
  (:import-from #:auth
                #:current-user)
  (:import-from #:clues/injectable
                #:injectable)
  (:import-from #:util/cookies
                #:cookies
                #:get-cookie
                #:set-cookie)
  (:export
   #:impersonate
   #:impersonatedp
   #:impersonation
   #:logout
   #:make-impersonation)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/impersonation)

(defclass impersonation ()
  ((cookies :inject cookies
            :initarg :cookies
            :reader cookies))
  (:metaclass injectable))

(defmethod impersonate ((self impersonation) user)
  (let ((admin-user (current-user)))
    (setf (current-user) user)
    (set-cookie (cookies self)
                "imp" "1")
    (setf (auth:session-value :admin-user)  admin-user)))

(defmethod impersonatedp ((self impersonation))
  (equal "1" (get-cookie (cookies self) "imp")))

(defmethod admin-user ((self impersonation))
  (auth:session-value :admin-user))

(defmethod logout ((self impersonation))
  (setf (auth:session-value :admin-user)  nil)
  (set-cookie (cookies self) "imp" ""))

(defun make-impersonation ()
  "In the past, we temporarily used a home-grown injector
mechanism. That's why this impersonation class is so un-idiomatically
designed."
  (make-instance 'impersonation
                 :cookies (make-instance 'cookies)))

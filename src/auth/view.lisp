;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/view
  (:use #:cl
        #:auth))
(in-package :auth/view)

(define-condition no-access-error (error)
  ((user :initarg :user
         :accessor error-user)
   (obj :initarg :obj
        :accessor error-obj)))

(defmethod print-object ((e no-access-error) out)
  (format out "User ~S can't access ~S" (error-user e) (error-obj e)))

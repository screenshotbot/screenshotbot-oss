;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/request
  (:use #:cl
        #:auth))
(in-package :auth/request)

(defclass authenticated-request (hunchentoot:request)
  ((user :initarg :user
         :initform nil
         :accessor request-user)
   (account :initarg :account
            :initform nil
            :accessor request-account
            :documentation "In screenshotbot this is called a `company`. But this is any account
 object that the user is accessing.")))

(defmethod authenticate-request (request))

(defmethod hunchentoot:acceptor-dispatch-request :around
    (acceptor
     (request authenticated-request))
  (authenticate-request request)
  (call-next-method))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/request
  (:use #:cl
        #:auth)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:core/installation/auth
                #:company-for-request))
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
  (auth:with-sessions ()
    (authenticate-request request)
    (call-next-method)))

(defmethod auth:authenticate-request ((request authenticated-request))
  (unless (auth:request-user request) ;; Might happen in tests
    (alexandria:when-let ((user (auth:session-value :user)))
      (setf (auth:request-user request) user)))
  (unless (auth:request-account request)
    (alexandria:when-let ((company (company-for-request *installation* request)))
      (setf (auth:request-account request) company))))


(defun current-user ()
  (and
   (boundp 'hunchentoot:*request*)
   (auth:request-user hunchentoot:*request*)))

(defun (setf current-user) (user &key expires-in)
  (setf (auth:session-value :user :expires-in expires-in) user)
  (setf (auth:request-user hunchentoot:*request*) user)
  user)

(defun logged-in-p ()
  (auth:current-user))

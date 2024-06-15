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
                #:company-for-request)
  (:local-nicknames (#:viewer-context #:auth/viewer-context)))
(in-package :auth/request)

(defclass authenticated-request (hunchentoot:request)
  ((user :initarg :user
         :initform nil
         :accessor request-user)
   (%viewer-context :initarg :viewer-context
                    :initform nil
                    :accessor auth:viewer-context)
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
      (setf (auth:request-user request) user)
      (setf (auth:viewer-context request)
            (make-instance 'viewer-context:viewer-context
                           :user user))
      (unless (auth:request-account request)
        (alexandria:when-let ((company (company-for-request *installation* request)))
          (cond
            ((auth:can-view company user)
             (setf (auth:request-account request) company))
            (t
             (warn "Could not set company for user: ~a, ~a"
                   company
                   user))))))))


(defun current-user ()
  (and
   (boundp 'hunchentoot:*request*)
   (auth:request-user hunchentoot:*request*)))

(defun (setf current-user) (user &key expires-in
                                   viewer-context)
  (setf (auth:session-value :user :expires-in expires-in) user)
  (setf (auth:request-user hunchentoot:*request*) user)
  (setf (auth:viewer-context hunchentoot:*request*)
        (or
         viewer-context
         (make-instance 'viewer-context:viewer-context
                        :user user)))
  user)

(defun logged-in-p ()
  (auth:current-user))

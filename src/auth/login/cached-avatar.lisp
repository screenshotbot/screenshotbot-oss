;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/login/cached-avatar
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:blob-pathname
                #:persistent-class
                #:blob)
  (:import-from #:auth/avatar
                #:content-type
                #:overriden-avatar
                #:overriden-avatar-for-user
                #:*overriden-avatar-lock*)
  (:import-from #:util/threading
                #:make-thread)
  (:import-from #:util/request
                #:http-success-response?
                #:http-request)
  (:import-from #:oidc/oidc
                #:access-token-str
                #:after-authentication)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:core/installation/installation
                #:*installation*))
(in-package :auth/login/cached-avatar)

(defclass cached-avatar-provider ()
  ()
  (:documentation "A mixin for an auth provider, where the avatar is cached at login time."))

(defvar *cv* (bt:make-condition-variable))

(defvar *token-logs* nil
  "For debugging T1832")

(defmethod after-authentication :after ((self cached-avatar-provider)
                                        &key email
                                          token
                                          avatar
                                        &allow-other-keys)
  (let ((user (auth:find-user *installation* :email email)))
    (make-thread
     (lambda ()
       (download-avatar user :token token :avatar avatar)))))


(auto-restart:with-auto-restart ()
  (defun download-avatar (user &key token avatar)
    (bt:with-lock-held (*overriden-avatar-lock*)
      (atomics:atomic-push (list user token avatar)
                           *token-logs*)
      (multiple-value-bind (res code headers)
          (http-request
           avatar
           :additional-headers `(("Authorization" . ,(format nil "Bearer ~a"
                                                             (access-token-str token))))
           :force-binary t)
        (unless (eql 404 code)
          (unless (http-success-response? code)
            (error  "Got response ~a when downloading avatar ~a" code avatar))
          (write-avatar user :res res :headers headers))))))

(defun write-avatar (user &key res headers)
  (let ((obj (or
              (overriden-avatar-for-user user)
              (make-instance 'overriden-avatar :user user))))
    (with-open-file (stream (blob-pathname obj)
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (write-sequence res stream)
      (setf (content-type obj)
            (assoc-value headers :content-type)))))

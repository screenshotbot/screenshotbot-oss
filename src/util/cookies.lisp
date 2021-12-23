;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/cookies
  (:use #:cl)
  (:import-from #:clues/injectable
                #:injectable)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:cookies
   #:get-cookie
   #:set-cookie))
(in-package :util/cookies)

(defclass cookies ()
  ((request :initform hunchentoot:*request* ;; todo: does it make sense to inject?
            :initarg :request
            :reader request)
   (reply :initform hunchentoot:*reply*
          :initarg :reply
          :reader reply)
   (host :initform (hunchentoot:host)
         :initarg :host
         :reader host)
   (proto :initform (hunchentoot:header-in* :x-forwarded-proto)
          :initarg :proto
          :reader proto))
  (:metaclass injectable))

(defun host-without-port (self)
  (car (str:split ":" (host self))))

(defmethod get-cookie ((self cookies) name)
  (hunchentoot:cookie-in name (request self)))

(defmethod set-cookie ((self cookies) name value &key)
  (let ((domain (host-without-port self)))
    (hunchentoot:set-cookie
     name :value value :domain domain :expires (+ (get-universal-time) 1000000)
          :path "/" :secure (string=
                             "https"
                             (proto self))
          :reply (reply self))))

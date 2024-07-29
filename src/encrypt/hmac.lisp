;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :encrypt/hmac
  (:use #:cl)
  (:import-from #:encrypt
                #:key-from-disk))
(in-package :encrypt/hmac)

(defvar *hmac-key* nil)

(defun hmac-key ()
  (util:or-setf
   *hmac-key*
   (key-from-disk "sha256-hmac-key.bin" 64)
   :thread-safe t))

(defmethod sign-hmac (bytes)
  (let ((mac (ironclad:make-mac :hmac
                                (hmac-key)
                                :sha256)))
    (ironclad:update-mac mac bytes)
    (ironclad:produce-mac mac)))

(defmethod sign-hmac ((str string))
  (sign-hmac (flex:string-to-octets str)))

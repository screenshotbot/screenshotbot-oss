;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/common-flags
  (:use #:cl
        #:com.google.flag)
  (:shadow #:define-flag)
  (:export
   #:*verbose*
   #:*api-key*
   #:*api-secret*
   #:*hostname*
   #:*help*
   #:*sdk-flags*
   #:define-flag))
(in-package :screenshotbot/sdk/common-flags)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *sdk-flags* (make-hash-table)))

(defmacro define-flag (name &rest args
     &key selector &allow-other-keys)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ,(intern (str:upcase selector) "KEYWORD") *sdk-flags*)
           ',name)
     (com.google.flag:define-flag ,name
       ,@args)))

(define-flag *verbose*
   :default-value nil
   :selector "verbose"
   :type boolean
  :help "Verbose logs")

(define-flag *api-key*
  :selector "api-key"
  :default-value nil
  :type (or null string)
  :help "Screenshotbot API Key. Defaults to $SCREENSHOTBOT_API_KEY.")

(define-flag *api-secret*
  :selector "api-secret"
  :default-value nil
  :type (or null string)
  :help "Screenshotbot API Secret. Defaults to $SCREENSHOTBOT_API_SECRET")

(define-flag *hostname*
  :selector "api-hostname"
  :default-value "https://api.screenshotbot.io"
  :type string
  :help "Screenshotbot API Endpoint"
  :documentation "Only used for Enterprise or Open Source users, Defaults to `https://api.screenshotbot.io` or $SCREENSHOTBOT_API_HOSTNAME")

(define-flag *help*
  :selector "help"
  :default-value nil
  :type boolean)

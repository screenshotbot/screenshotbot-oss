;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/api-context
  (:use #:cl)
  (:export
   #:api-context
   #:key
   #:secret
   #:hostname
   #:desktop-api-context
   #:desktop-p))
(in-package :screenshotbot/sdk/api-context)

(defclass api-context ()
  ((key :initarg :key
        :reader key)
   (secret :initarg :secret
           :reader secret)
   (hostname :initarg :hostname
             :reader hostname
             :documentation "A URL like https://screenshotbot.io")))

(defclass desktop-api-context ()
  ((hostname :reader hostname
             :initform "http://localhost:4095")))

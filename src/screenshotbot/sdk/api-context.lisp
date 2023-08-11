;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/api-context
  (:use #:cl)
  (:import-from #:screenshotbot/api/model
                #:*api-version*)
  (:export
   #:api-context
   #:key
   #:secret
   #:hostname
   #:desktop-api-context
   #:desktop-p
   #:remote-version))
(in-package :screenshotbot/sdk/api-context)

(defclass base-api-context ()
  ((remote-version :initform *api-version*
                   :accessor remote-version)))

(defclass api-context (base-api-context)
  ((key :initarg :key
        :reader key)
   (secret :initarg :secret
           :reader secret)
   (hostname :initarg :hostname
             :reader hostname
             :documentation "A URL like https://screenshotbot.io")))

(defclass desktop-api-context (base-api-context)
  ((hostname :reader hostname
             :initform "http://localhost:4095")))

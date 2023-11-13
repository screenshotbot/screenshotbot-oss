;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/api-context
  (:use #:cl)
  (:import-from #:screenshotbot/api/model
                #:*api-version*)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:export
   #:api-context
   #:key
   #:secret
   #:hostname
   #:desktop-api-context
   #:desktop-p
   #:remote-version
   #:fetch-version))
(in-package :screenshotbot/sdk/api-context)

(defgeneric fetch-version (api-context)
  (:documentation "Actually fetch the api version, without any caching. Don't call this directly, instead call REMOTE-VERSION on the api-context."))

(defclass base-api-context ()
  ((remote-version :initform nil
                   :initarg :remote-version
                   :accessor remote-version)))

(defmethod remote-version :around ((self base-api-context))
  (let ((prev (call-next-method)))
    (cond
      (prev  prev)
      (t
       (setf (remote-version self) (fetch-version self))))))

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

(defclass json-api-context (api-context)
  ((hostname :initarg :hostname
             :json-key "hostname"
             :json-type :string
             :reader hostname)
   (key :initarg :key
            :json-key "apiKey"
            :json-type :string
            :reader key)
   (secret :initarg :secret
           :json-key "apiSecretKey"
           :json-type :string
           :reader secret))
  (:metaclass ext-json-serializable-class)
  (:documentation "An API context that can be serialized"))

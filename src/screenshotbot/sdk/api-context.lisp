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
  (:import-from #:util/request
                #:engine
                #:*engine*)
  (:import-from #:util/reused-ssl
                #:reused-ssl-mixin)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/sdk/hostname
                #:format-api-url
                #:api-hostname)
  (:export
   #:api-context
   #:key
   #:secret
   #:hostname
   #:remote-version
   #:fetch-version
   #:engine
   #:api-feature-enabled-p)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/api-context)

(defgeneric fetch-version (api-context)
  (:documentation "Actually fetch the api version, without any caching. Don't call this directly, instead call REMOTE-VERSION on the api-context."))

(defclass base-api-context ()
  ((remote-version :initform nil
                   :initarg :remote-version
                   :accessor remote-version)
   (features :initarg :features
             :initform nil
             :initarg api-features
             :accessor api-features)))

(defmethod api-context-prepared-p ((self base-api-context))
  (slot-value self 'remote-version))

(defmethod fetch-remote-information ((self base-api-context))
  (let ((prev (slot-value self 'remote-version)))
    (cond
      (prev  prev)
      (t
       (multiple-value-bind (version-num version-obj)
           (fetch-version self)
         (setf (remote-version self) version-num)
         (setf (api-features self) (?. dto:api-features version-obj)))))))

(defmethod remote-version :before ((self base-api-context))
  (fetch-remote-information self))

(defmethod api-features :before ((self base-api-context))
  (fetch-remote-information self))

(defmethod api-feature-enabled-p ((self base-api-context)
                                  feature)
  (str:s-member
   (api-features self)
   (str:downcase feature)))

(defclass api-engine (reused-ssl-mixin engine)
  ())

(defvar *api-engine* (make-instance 'api-engine))

(defclass api-context (base-api-context)
  ((key :initarg :key
        :initform nil
        :reader key)
   (secret :initarg :secret
           :initform nil
           :reader secret)
   (hostname :initarg :hostname
             :reader hostname
             :writer (setf %hostname)
             :documentation "A URL like https://screenshotbot.io")
   (engine :initarg :engine
           :reader engine
           :initform *api-engine*)))

(defun %fix-hostname (found-hostname)
  (cond
    ((str:emptyp found-hostname)
     nil)
    (t
     (api-hostname
      :hostname found-hostname))))

(defun format-api-url (api-context api)
  (quri:render-uri
   (quri:merge-uris
    api
    (api-hostname :hostname (hostname api-context)))))

(defmethod initialize-instance :after ((self api-context) &rest args &key hostname)
  (cond
    ((str:emptyp hostname)
     (setf (%hostname self) "https://api.screenshotbot.io"))
    (t
     (setf (%hostname self)
           (%fix-hostname (hostname self)))))
  (log:debug "Using hostname: ~a" (hostname self)))

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

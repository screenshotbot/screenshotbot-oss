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
                #:http-request-impl
                #:http-request
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
   #:api-feature-enabled-p
   #:session-id
   #:extract-hostname-from-secret)
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
             :accessor api-features)
   (session-id :initform (format nil "~a-~a-~a"
                                 (uiop:hostname)
                                 (local-time:now)
                                 (random 1000000))
               :reader session-id)))

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
  ((session-id :initarg :session-id
               :reader session-id)))

(defmethod http-request-impl ((self api-engine) url &rest args &key additional-headers)
  (apply #'call-next-method
         self
         url
         :additional-headers
         (list*
          (cons :x-cli-session-id (session-id self))
          additional-headers)
         args))

(defvar *api-engine* (make-instance 'api-engine))

(defclass api-context (base-api-context)
  ((key :initarg :key
        :initform nil
        :reader key
        :writer (setf %key))
   (secret :initarg :secret
           :initform nil
           :reader secret
           :writer (setf %secret))
   (hostname :initarg :hostname
             :reader hostname
             :writer (setf %hostname)
             :documentation "A URL like https://screenshotbot.io")
   (engine :accessor engine
           :initform nil
           :initarg :engine)))



(defun %fix-hostname (found-hostname)
  (cond
    ((str:emptyp found-hostname)
     nil)
    (t
     (api-hostname
      :hostname found-hostname))))

(defun extract-info-from-secret (api-secret)
  "Extract the api-key, secret, and hostname from an encoded API secret.
Returns three values: (api-key actual-secret hostname), or NIL for all if decoding fails."
  (when (and api-secret (> (length api-secret) 40))
    (handler-case
        (let* ((secret-start (- (length api-secret) 40))
               (encoded (str:substring 0 secret-start api-secret))
               (actual-secret (str:substring secret-start nil api-secret))
               (decoded (base64:base64-string-to-string encoded))
               (parts (str:split "," decoded)))
          (when (>= (length parts) 3)
            (values (first parts) actual-secret (third parts))))
      (error (e)
        ;; If decoding fails, log a warning since the secret is longer than expected
        (log:warn "Failed to decode API secret (length: ~a): ~a. The API secret might be invalid, did you copy it in full?"
                  (length api-secret) e)
        (values nil nil nil)))))

(defun extract-hostname-from-secret (api-secret)
  "Extract the hostname from an encoded API secret if possible.
Returns NIL if the secret doesn't contain hostname information."
  (multiple-value-bind (key secret hostname)
      (extract-info-from-secret api-secret)
    (declare (ignore key secret))
    hostname))

(defun format-api-url (api-context api)
  (quri:render-uri
   (quri:merge-uris
    api
    (api-hostname :hostname (hostname api-context)))))

(defmethod initialize-instance :after ((self api-context) &rest args &key hostname)
  ;; Try to extract api-key from the provided secret if key is not provided
  (when (str:emptyp (key self))
    (multiple-value-bind (extracted-key extracted-secret extracted-hostname)
        (extract-info-from-secret (secret self))
      (declare (ignore extracted-secret extracted-hostname))
      (when extracted-key
        (log:debug "Extracted API key from API secret")
        (setf (%key self) extracted-key))))

  (cond
    ((str:emptyp hostname)
     ;; Try to extract hostname from the secret first
     (let ((extracted-hostname (extract-hostname-from-secret (secret self))))
       (cond
         ((and extracted-hostname (not (str:emptyp extracted-hostname)))
          (log:debug "Extracted hostname from API secret: ~a" extracted-hostname)
          (setf (%hostname self) (%fix-hostname extracted-hostname)))
         (t
          (setf (%hostname self) "https://api.screenshotbot.io")))))
    (t
     (setf (%hostname self)
           (%fix-hostname (hostname self)))))
  (log:debug "Using hostname: ~a" (hostname self))
  (unless (engine self)
    (setf (engine self)
          (make-instance 'api-engine
                         :session-id (session-id self)))))

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

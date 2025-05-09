;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/request
  (:use #:cl)
  (:import-from #:screenshotbot/api/model
                #:*api-version*
                #:encode-json)
  (:import-from #:util/request
                #:engine
                #:http-request)
  (:import-from #:screenshotbot/sdk/hostname
                #:format-api-url)
  (:import-from #:screenshotbot/sdk/version-check
                #:*client-version*
                #:remote-supports-basic-auth-p)
  (:import-from #:screenshotbot/sdk/backoff
                #:maybe-retry-request)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class
                #:json-mop-to-string)
  (:import-from #:anaphora
                #:it
                #:awhen)
  (:import-from #:alexandria
                #:assoc-value
                #:when-let)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:dto #:screenshotbot/api/model)
                    (#:e #:screenshotbot/sdk/env)
                    (#:api-context #:screenshotbot/sdk/api-context)
                    (#:android   #:screenshotbot/sdk/android)
                    (#:run-context #:screenshotbot/sdk/run-context))
  (:export
   #:api-error
   #:ensure-api-success
   #:request))
(in-package :screenshotbot/sdk/request)

(define-condition api-error (error)
  ((message :initarg :message)))

(defmethod print-object ((e api-error) stream)
  (with-slots (message) e
   (format stream "#<API-ERROR ~a>" message)))


(defun ensure-api-success (result)
  (let ((indent "    "))
   (awhen (assoc-value result :error)
     (log:error "API error: ~a" it)
     (when-let ((stacktrace (assoc-value result :stacktrace)))
      (log:error "Server stack trace: ~%~a~a"
                 indent
                 (str:join (format nil "~%~a" indent)
                           (str:lines stacktrace))))
     (error 'api-error :message it)))
  (assoc-value result :response))




(defmethod %make-basic-auth (api-context)
  (list
   (api-context:key api-context)
   (api-context:secret api-context)))


(auto-restart:with-auto-restart (:attempt attempt)
  (defun %request (api-context
                   api &key (method :post)
                         parameters
                         content
                         (auto-retry t)
                         (backoff 2))
    ;; TODO: we're losing the response code here, we need to do
    ;; something with it.

    (when (and auto-retry
               (streamp content))
      (warn "auto-retry won't work for stream content"))

    (multiple-value-bind (stream response-code)
        (handler-bind ((error (lambda (e)
                                (warn "Retrying request because of error, is this a timeout? ~a" e)
                                (when auto-retry
                                  (maybe-retry-request
                                   408 ;; This is most likely the case this low down
                                   :attempt attempt
                                   :restart 'retry-%request
                                   :errorp nil
                                   :backoff backoff)))))
          (http-request
           (format-api-url api-context api)
           :method method
           :want-stream t
           :method method
           :basic-authorization (when (remote-supports-basic-auth-p api-context)
                                  (%make-basic-auth api-context))
           :additional-headers `(("X-client-version" . ,*client-version*)
                                 ("X-client-api-version" . ,*api-version*))
           :content content
           :external-format-out :utf-8
           :read-timeout 45
           :engine (api-context:engine api-context)
           :parameters (cond
                         ((remote-supports-basic-auth-p api-context)
                          parameters)
                         (t (list*
                             (cons "api-key" (api-context:key api-context))
                             (cons "api-secret-key" (api-context:secret api-context))
                             parameters)))))
      (when auto-retry
        (maybe-retry-request
         response-code
         :attempt attempt
         :restart 'retry-%request
         :backoff backoff))

      (with-open-stream (stream stream)
        (values
         (uiop:slurp-input-stream 'string stream)
         response-code)))))



(defmethod request ((api-context api-context:api-context)
                    api &key (method :post)
                          parameters
                          (decode-response t)
                          content)
  (log:debug "Making API request: ~S" api)
  (multiple-value-bind (json code)
      (%request api-context
                api :method method
                :parameters parameters
                :content (cond
                           ((or
                             (eql :put method)
                             (typep (class-of content)
                                    'ext-json-serializable-class))
                            (json-mop-to-string
                             content))
                           ((and (eql method :post)
                                 content)
                            content)))
    (cond
      (decode-response
       (handler-case
           (let ((result (json:decode-json-from-string json)))
             (ensure-api-success result))
         (json:json-syntax-error (e)
           (error "Could not parse json:"
                  json))))
      (t
       (values json code)))))



;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/http
  (:use #:cl)
  (:import-from #:auth-server/client/conditions
                #:http-error
                #:oauth-error)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:decode-json
   #:field
   #:post-form
   #:get-json))
(in-package :auth-server/client/http)

(defun decode-json (string)
  "Decode JSON into an alist whose keys are the wire names, verbatim.

CL-JSON would otherwise mangle `access_token` on the way in, and OAuth
member names are fixed by the RFCs -- they are not ours to camel-case."
  (let ((json:*json-identifier-name-to-lisp* #'identity)
        (json:*identifier-name-to-key* #'identity))
    (json:decode-json-from-string string)))

(defun field (alist name)
  (a:assoc-value alist name :test #'equal))

(defun %body-string (body)
  "Drakma hands back octets for content types it doesn't consider text,
and application/json is one of them."
  (if (stringp body)
      body
      (flexi-streams:octets-to-string body :external-format :utf-8)))

(defun %parse-response (url body status &key allow-empty)
  (let* ((text (%body-string body))
         (payload (ignore-errors (decode-json text))))
    (cond
      ((<= 200 status 299)
       (cond
         (payload payload)
         ;; RFC 7009 §2.2, for one, specifies an empty 200.
         (allow-empty nil)
         (t
          (error 'http-error :status status :url url
                             :body "expected a JSON body"))))
      ;; RFC 6749 §5.2: errors arrive as a 4xx *with* a JSON body naming
      ;; the error, so the body is the interesting part, not the status.
      ((and payload (field payload "error"))
       (error 'oauth-error
              :code (field payload "error")
              :description (field payload "error_description")))
      (t
       (error 'http-error :status status :url url
                          :body (str:shorten 300 text))))))

(defun post-form (url params &key allow-empty)
  "POST an application/x-www-form-urlencoded body, decoding the JSON reply.

PARAMS is an alist of strings. Entries with a NIL value are dropped, so
callers can pass optional parameters unconditionally. ALLOW-EMPTY permits
a successful response to carry no body at all."
  (let ((params (loop for (key . value) in params
                      if value
                        collect (cons key value))))
    (multiple-value-bind (body status)
        (drakma:http-request url
                             :method :post
                             :parameters params
                             ;; A 302 from a token endpoint is never
                             ;; something we should quietly follow.
                             :redirect nil
                             :external-format-out :utf-8)
      (%parse-response url body status :allow-empty allow-empty))))

(defun get-json (url &key bearer)
  (multiple-value-bind (body status)
      (drakma:http-request url
                           :method :get
                           :redirect nil
                           :additional-headers
                           (append
                            '(("Accept" . "application/json"))
                            (when bearer
                              `(("Authorization" . ,(format nil "Bearer ~a" bearer))))))
    (%parse-response url body status)))

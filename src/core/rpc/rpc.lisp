;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/rpc/rpc
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:encode
                #:decode
                #:persistent-class
                #:store-object)
  (:import-from #:screenshotbot/model/api-key
                #:generate-api-secret)
  (:import-from #:util/store/store
                #:with-class-validation)
  (:import-from #:encrypt/hmac
                #:encode-signed-string
                #:decode-signed-string)
  (:import-from #:util/store/encodable
                #:encodable)
  (:import-from #:util/request
                #:http-request)
  (:export
   #:authenticate-rpc-request))
(in-package :core/rpc/rpc)

(defclass rpc-acceptor-mixin ()
  ())

(with-class-validation
  (defclass rpc-auth-id (store-object)
    ((%secret :initarg :secret
              :reader secret)
     (%created-at :initarg :created-at))
    (:metaclass persistent-class)
    (:default-initargs :secret (generate-api-secret)
                      :created-at (get-universal-time))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor rpc-acceptor-mixin)
                                                  request)
  (cond
    ((and
      (eql :post (hunchentoot:request-method request))
      (equal "/intern/rpc" (hunchentoot:script-name request)))
     (perform-rpc request))
    (t
     (call-next-method))))

(define-condition rpc-authentication-failed (error)
  ())

(defun authenticate-rpc-request (request)
  (multiple-value-bind (key secret) (hunchentoot:authorization request)
    (let ((key (parse-integer key :junk-allowed t)))
      (unless key
        (error 'rpc-authentication-failed))
      (let ((auth-id (bknr.datastore:store-object-with-id key)))
        (unless (typep auth-id 'rpc-auth-id)
          (error 'rpc-authentication-failed))
        (unless (equal secret (secret auth-id))
          (error 'rpc-authentication-failed))))))

(defun encode-bknr-object (obj)
  (let ((stream (flex:make-in-memory-output-stream)))
    (encode obj stream)
    (let* ((base64 (base64:usb8-array-to-base64-string
                    (flex:get-output-stream-sequence stream)))
           (content (encode-signed-string base64)))
      content)))

(defun decode-bknr-object (body)
  (let ((base64 (decode-signed-string body)))
    (let ((arr (base64:base64-string-to-usb8-array base64)))
      (decode (flex:make-in-memory-input-stream arr)))))

(defmethod perform-rpc (request)
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (log:info "Delegating request: ~a" request)
    (let ((result
            (call-rpc
             (decode-bknr-object body))))
      (encode-bknr-object result))))

(defclass hello-world-rpc (encodable)
  ())

(defmethod call-rpc ((self hello-world-rpc))
  (log:info "hello world!!")
  "123")

(defun send-rpc (url rpc)
  (decode-bknr-object
   (http-request
    url
    :method :post
    :content (encode-bknr-object rpc))))

;;(send-rpc "https://staging.screenshotbot.io/intern/rpc" (make-instance 'hello-world-rpc))

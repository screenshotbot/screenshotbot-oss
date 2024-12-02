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

(with-class-validation
  (defclass rpc-auth-id (store-object)
    ((%secret :initarg :secret
              :reader secret)
     (%created-at :initarg :created-at))
    (:metaclass persistent-class)
    (:default-initargs :secret (generate-api-secret)
                      :created-at (get-universal-time))))

(hunchentoot:define-easy-handler (rpc-handler :uri "/intern/rpc") ()
  (perform-rpc hunchentoot:*request*))


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

(defun assert-direct-request (request)
  "RPC requests shouldn't go through LB or Nginx."
  (assert (eql :post (hunchentoot:request-method request)))
  (assert (not (hunchentoot:header-in :x-forwarded-for request)))
  (assert (not (hunchentoot:header-in :x-real-ip request))))

(defmethod perform-rpc (request)
  (assert-direct-request request)
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

(defun send-rpc (url rpc &key (read-timeout 600))
  (decode-bknr-object
   (http-request
    url
    :method :post
    :read-timeout read-timeout
    :content (encode-bknr-object rpc))))

(defun %servers ()
  (typecase bknr.datastore:*store*
    #+bknr.cluster
    (bknr.cluster/server::lisp-state-machine
     (loop for conf in (bknr.cluster/server:list-peers bknr.datastore:*store*)
           collect (first (str:rsplit ":" conf :limit 3))))
    (t
     (list "127.0.0.1"))))

(defun %port ()
  (hunchentoot:acceptor-port server::*multi-acceptor*))

(defun map-rpc (rpc &key (read-timeout 600))
  "Map the RPC across all the peers in the cluster (including the current-one)"
  (let* ((servers (%servers))
         (results (make-array (length servers))))
    (mapc
     #'bt:join-thread
     (loop for server in servers
           for i from 0
           collect
           (let ((i i)
                 (server server))
             (util/threading:make-thread
              (lambda ()
                (setf
                 (aref results i)
                 (send-rpc
                  (format nil "http://~a:~a/intern/rpc"
                          server
                          (%port))
                  rpc
                  :read-timeout read-timeout)))))))
    (loop for result across results
          collect result)))

;; (map-rpc (make-instance 'hello-world-rpc))

;;(send-rpc "http://localhost:4001/intern/rpc" (make-instance 'hello-world-rpc))

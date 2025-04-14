;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/hunchentoot-engine
  (:use #:cl)
  (:import-from #:util/request
                #:http-request-impl)
  (:import-from #:hunchentoot
                #:acceptor-reply-class
                #:acceptor-request-class))
(in-package :util/hunchentoot-engine)

(defclass hunchentoot-engine ()
  ((acceptor :initarg :acceptor
             :reader acceptor))
  (:documentation "A request engine that dispatches to a hunchentoot acceptor instead."))

(defun compute-headers-in (&key basic-authorization content additional-headers)
  (append
   additional-headers
   (remove-if
    #'null
    `(,(when (stringp content)
         `(:content-length . ,(format nil "~a" (length content))))
      ,(when basic-authorization
         `(:authorization . ,(format nil "Basic ~a"
                                     (base64:string-to-base64-string
                                      (format nil "~a:~a"
                                              (first basic-authorization)
                                              (second basic-authorization))))))))))

(defun %make-uri (uri &key parameters)
  (let* ((uri (quri:uri uri))
         (params (quri:uri-query-params uri)))
    (setf (quri:uri-query-params uri)
          (append
           parameters
           params))
    (quri:render-uri uri)))

(defmethod http-request-impl ((self hunchentoot-engine)
                              url &key method basic-authorization want-stream
                                    parameters
                                    content
                                    force-binary
                                    additional-headers
                              &allow-other-keys)
  (let* ((acceptor (acceptor self))
         (hunchentoot:*acceptor* acceptor)
         (content-stream (cond
                           ((and
                             (streamp content)
                             (equal '(unsigned-byte 8) (stream-element-type content)))
                            content)
                           ((stringp content)
                            (flex:make-in-memory-input-stream
                             (flex:string-to-octets content
                                                    :external-format :utf-8)))
                           (content
                            (error "Content of type ~a, not supported by hunchentoot-engine"
                                   content))
                           (t
                            (flex:make-in-memory-input-stream
                             (make-array 0 :element-type 'flex:octet)))))
         (request (make-instance (acceptor-request-class acceptor)
                                 :acceptor acceptor
                                 :local-addr "127.0.0.1"
                                 :local-port 9999
                                 :remote-addr "127.0.0.1"
                                 :remote-port 9998
                                 :headers-in (compute-headers-in
                                              :content content
                                              :basic-authorization basic-authorization
                                              :additional-headers additional-headers)
                                 :content-stream content-stream
                                 :uri (%make-uri
                                       url
                                       :parameters parameters)
                                 :method method
                                 :server-protocol :http))
         (hunchentoot:*request* request)
         (reply (make-instance (acceptor-reply-class acceptor)))
         (hunchentoot:*reply* reply))
    (let ((body
            (let ((hunchentoot::*hunchentoot-stream* content-stream))
             (hunchentoot:acceptor-dispatch-request acceptor request))))
      (values
       (cond
         ((and want-stream
               force-binary)
          (flex:make-in-memory-input-stream
           (flex:string-to-octets body)))
         (want-stream
          (make-string-input-stream
           body))
         (t
          body))
       (hunchentoot:return-code reply)
       (hunchentoot:headers-out* reply)))))



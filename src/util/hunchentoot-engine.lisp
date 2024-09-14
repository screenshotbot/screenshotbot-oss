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


(defmethod http-request-impl ((self hunchentoot-engine)
                              url &key method &allow-other-keys)
  (let* ((acceptor (acceptor self))
         (hunchentoot:*acceptor* acceptor)
         (request (make-instance (acceptor-request-class acceptor)
                                 :acceptor acceptor
                                 :local-addr "127.0.0.1"
                                 :local-port 9999
                                 :remote-addr "127.0.0.1"
                                 :remote-port 9998
                                 :headers-in nil
                                 :content-stream
                                 (flex:make-in-memory-input-stream
                                  (make-array 0 :element-type 'flex:octet))
                                 :uri url
                                 :method method
                                 :server-protocol :http))
         (hunchentoot:*request* request)
         (reply (make-instance (acceptor-reply-class acceptor)))
         (hunchentoot:*reply* reply))
    (values
     (hunchentoot:acceptor-dispatch-request acceptor request)
     (hunchentoot:return-code reply)
     (hunchentoot:headers-out* reply))))



;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/random-port
  (:use #:cl)
  (:import-from #:hunchentoot
                #:acceptor-port
                #:start-listening)
  (:import-from #:hunchentoot-extensions/existing-socket
                #:existing-socket
                #:acceptor-with-existing-socket))
(in-package :hunchentoot-extensions/random-port)

(defclass acceptor-on-random-port (acceptor-with-existing-socket)
  ()
  (:default-initargs :port 0))

(defmethod start-listening :before ((acceptor acceptor-on-random-port))
  #+lispworks
  (setf (existing-socket acceptor)
        (comm::create-tcp-socket-for-service
         0 :address "127.0.0.1" :backlog 30))
  #-lispworks
  (let* ((usocket (usocket:socket-listen "127.0.0.1" 0
                                         :element-type '(unsigned-byte 8))))
    (setf (existing-socket acceptor)
          usocket)))


;; On usocket based implementations, hunchentoot already supports 0 as
;; the acceptor port.
#+lispworks
(defmethod acceptor-port ((acceptor acceptor-on-random-port))
  (cond
    ((existing-socket acceptor)
     (nth-value
      1
      (comm:get-socket-address (existing-socket acceptor))))
    (t
     0)))

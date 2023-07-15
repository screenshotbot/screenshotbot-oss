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
  ())

(defmethod start-listening :before ((acceptor acceptor-on-random-port))
  (let* ((usocket (usocket:socket-listen "127.0.0.1" 0))
         (socket (usocket:socket usocket)))
    (setf (existing-socket acceptor) socket)))

(defmethod acceptor-port ((acceptor acceptor-on-random-port))
  (cond
    ((existing-socket acceptor)
     (nth-value
      1
      (comm:get-socket-address (existing-socket acceptor))))
    (t
     0)))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :server/acceptor-override
  (:use #:cl
        #:hunchentoot)
  (:import-from #:hunchentoot
                #:acceptor-process
                #:start-listening
                #:acceptor-taskmaster
                #:acceptor-listen-backlog
                #:handle-incoming-connection
                #:acceptor-shutdown-p
                #:acceptor-address
                #:acceptor-port)
  (:export
   #:ipv6-acceptor))
(in-package :server/acceptor-override)

(defclass ipv6-acceptor (acceptor)
  ()
  (:default-initargs
   :address "::"))

(defmethod start-listening ((acceptor ipv6-acceptor))
  (multiple-value-bind (listener-process startup-condition)
      (comm:start-up-server :service (acceptor-port acceptor)
                            :address (acceptor-address acceptor)
                            :process-name (format nil "Hunchentoot listener \(~A:~A)"
                                                  (or (acceptor-address acceptor) "*")
                                                  (acceptor-port acceptor))
                            #-(or :lispworks4 :lispworks5 :lispworks6)
                            :backlog
                            #-(or :lispworks4 :lispworks5 :lispworks6)
                            (acceptor-listen-backlog acceptor)
                            ;; this function is called once on startup - we
                            ;; use it to check for errors and random port
                            :announce (lambda (socket &optional condition)
                                        (when condition
                                          (error condition))
                                        (when (or (null (acceptor-port acceptor))
                                                  (zerop (acceptor-port acceptor)))
                                          (multiple-value-bind (address port)
                                              (comm:get-socket-address socket)
                                            (declare (ignore address))
                                            (setf (slot-value acceptor 'port) port))))
                            ;; this function is called whenever a connection
                            ;; is made
                            :function (lambda (handle)
                                        (unless (acceptor-shutdown-p acceptor)
                                          (handle-incoming-connection
                                           (acceptor-taskmaster acceptor) handle)))
                            ;; wait until the acceptor was successfully started
                            ;; or an error condition is returned
                            :ipv6 :both
                            :wait t)
    (when startup-condition
      (error startup-condition))
    (mp:process-stop listener-process)
    (setf (acceptor-process acceptor) listener-process)
    (values)))


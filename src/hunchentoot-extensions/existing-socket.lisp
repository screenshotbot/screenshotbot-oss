;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/existing-socket
  (:use #:cl)
  (:import-from #:hunchentoot
                #:acceptor-process
                #:acceptor-taskmaster
                #:handle-incoming-connection
                #:acceptor-shutdown-p
                #:start-listening)
  (:export
   #:existing-socket
   #:acceptor-with-existing-socket))
(in-package :hunchentoot-extensions/existing-socket)

(defclass acceptor-with-existing-socket ()
  ((existing-socket :initarg :existing-socket
                    :initform nil
                    :accessor existing-socket)))

(defmethod start-listening ((acceptor acceptor-with-existing-socket))
  (cond
    ((not (existing-socket acceptor))
     (call-next-method))
    (t
     (let ((process
             (bt:make-thread
              (lambda ()
                (loop until (acceptor-shutdown-p acceptor)
                      do
                         (handle-incoming-connection
                          (acceptor-taskmaster acceptor)
                          (comm::get-fd-from-socket (existing-socket acceptor))))))))
       ;; This will get unstopped in accept-connections
       (mp:process-stop process)
       (setf (acceptor-process acceptor) process)
       (values)))))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/cluster/leadership
  (:use #:cl)
  (:import-from #:clingon
                #:getopt
                #:make-option)
  (:import-from #:server/cluster/status
                #:get-pid
                #:eval-on-pid)
  (:export #:cluster-leadership/command))
(in-package :server/cluster/leadership)

(defun cluster-leadership/command ()
  (clingon:make-command :name "leadership"
                        :description "Cluster leadership management"
                        :handler (lambda (cmd)
                                   (clingon:print-usage-and-exit cmd t))
                        :sub-commands (list
                                       (leadership-acquire/command)
                                       (leadership-release/command))))

(defun leadership-acquire/command ()
  (clingon:make-command :name "acquire"
                        :description "Acquire cluster leadership for this node"
                        :handler #'leadership-acquire/handler
                        :options nil))

(defun leadership-acquire/handler (cmd)
  (declare (ignore cmd))
  (let ((pid (get-pid)))
    (cond
      (pid
       #+lispworks
       (handler-case
           (progn
             (format t "Requesting leadership for this node (PID ~a)...~%" pid)
             (eval-on-pid pid
                          '(let ((store bknr.datastore:*store*))
                            (bknr.cluster/server::bknr-vote store 1000)
                            (format t "Vote initiated.~%"))))
         (error (e)
           (format t "Error: ~a~%" e)))
       #-lispworks
       (format t "Leadership commands only supported on LispWorks~%"))
      (t
       (format t "No running server found.~%"))))
  (values))

(defun leadership-release/command ()
  (clingon:make-command :name "release"
                        :description "Release cluster leadership (transfer to another node)"
                        :handler #'leadership-release/handler
                        :options nil))

(defun leadership-release/handler (cmd)
  (declare (ignore cmd))
  (let ((pid (get-pid)))
    (cond
      (pid
       #+lispworks
       (handler-case
           (progn
             (format t "Releasing leadership for PID ~a...~%" pid)
             (eval-on-pid pid
                          '(let ((store bknr.datastore:*store*))
                            (bknr.cluster/server::bknr-transfer-leader store)
                            (format t "Leadership released.~%"))))
         (error (e)
           (format t "Error: ~a~%" e)))
       #-lispworks
       (format t "Leadership commands only supported on LispWorks~%"))
      (t
       (format t "No running server found.~%"))))
  (values))

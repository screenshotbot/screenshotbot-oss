;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/cluster/peers
  (:use #:cl)
  (:import-from #:clingon
                #:getopt
                #:make-option
                #:command-arguments)
  (:import-from #:server/cluster/status
                #:get-pid
                #:eval-on-pid)
  (:import-from #:str
                #:rsplit
                #:starts-with-p
                #:ends-with-p)
  (:export #:add-peer/command
           #:remove-peer/command
           #:peer-to-ip))
(in-package :server/cluster/peers)

(defun ip-to-peer (ip)
  "Convert an IP address to a braft peer string by appending the default port and index.
If the IP is IPv6 (contains a colon), it is wrapped in square brackets.
The suffix :7070:0:0 is used to match the observed cluster configuration."
  (if (find #\: ip)
      (format nil "[~a]:7070:0:0" ip)
      (format nil "~a:7070:0:0" ip)))

(defun add-peer/command ()
  (clingon:make-command
   :name "add-peer"
   :description "Add a peer to the cluster"
   :usage "PEER-IP"
   :handler #'add-peer/handler
   :options nil))

(defun add-peer/handler (cmd)
  (let ((args (command-arguments cmd)))
    (unless (= 1 (length args))
      (format t "Usage: screenshotbot cluster add-peer <peer-ip>~%")
      (uiop:quit 1))
    (let ((peer (ip-to-peer (first args)))
          (pid (get-pid)))
      (cond
        (pid
         #+lispworks
         (handler-case
             (eval-on-pid
              pid
              `(let* ((store bknr.datastore:*store*))
                 (let ((peers (bknr.cluster/server:list-peers store)))
                   (if (member ,peer peers :test #'string=)
                       (format t "Peer ~a is already in the cluster.~%" ,peer)
                       (progn
                         (bknr.cluster/server:update-conf store (cons ,peer peers))
                         (format t "Added peer ~a.~%" ,peer))))))
           (error (e)
             (format t "Error: ~a~%" e)))
         #-lispworks
         (format t "Cluster commands only supported on LispWorks~%"))
        (t (format t "No running server found.~%"))))))

(defun remove-peer/command ()
  (clingon:make-command
   :name "remove-peer"
   :description "Remove a peer from the cluster"
   :usage "PEER-IP"
   :handler #'remove-peer/handler
   :options nil))

(defun remove-peer/handler (cmd)
  (let ((args (command-arguments cmd)))
    (unless (= 1 (length args))
      (format t "Usage: screenshotbot cluster remove-peer <peer-ip>~%")
      (uiop:quit 1))
    (let ((peer (ip-to-peer (first args)))
          (pid (get-pid)))
      (cond
        (pid
         #+lispworks
         (handler-case
             (eval-on-pid
              pid
              `(let* ((store bknr.datastore:*store*))
                 (let ((peers (bknr.cluster/server:list-peers store)))
                   (if (not (member ,peer peers :test #'string=))
                       (format t "Peer ~a is not in the cluster.~%" ,peer)
                       (progn
                         (bknr.cluster/server:update-conf store (remove ,peer peers :test #'string=))
                         (format t "Removed peer ~a.~%" ,peer))))))
           (error (e)
             (format t "Error: ~a~%" e)))
         #-lispworks
         (format t "Cluster commands only supported on LispWorks~%"))
        (t (format t "No running server found.~%"))))))

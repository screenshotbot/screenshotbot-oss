;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/aws-store
  (:use #:cl)
  (:import-from #:util/request
                #:http-request)
  (:export
   #:make-aws-store))
(in-package :util/store/aws-store)

;; This isn't meant to be used outside of Modern Interpreters. This is
;; highly tied to our deployment infrastructure.

(defun get-current-ip ()
  (http-request "http://169.254.169.254/latest/meta-data/local-ipv4"
                :want-string t))

(defun make-aws-store (&key private-ips
                         (group (error "must provide group, and it must be equal to the name of the current USER."))
                         port)
  (assert (not (equal "root" (uiop:getenv "USER"))))
  (assert (equal group (uiop:getenv "USER")))
  (let ((current-ip (get-current-ip)))
    (util/store/store:make-default-store
     'util/store/store:raft-store-final
     :data-path (path:catdir
                 (user-homedir-pathname)
                 "raft-data/")
     :port port
     :ip current-ip
     :priority (cond
                 ((equal current-ip (first private-ips))
                  1)
                 (t
                  0))
     :config (str:join ","
                       (loop for ip in private-ips
                             collect (format nil "~a:~a:0" ip port)))
     :group group
     :election-timeout-ms 1000)))

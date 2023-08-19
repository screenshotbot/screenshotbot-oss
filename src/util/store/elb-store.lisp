;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/elb-store
  (:use #:cl)
  (:import-from #:bknr.cluster/server
                #:on-leader-start
                #:on-leader-stop)
  (:import-from #:util/threading
                #:ignore-and-log-errors)
  (:export
   #:elb-store-mixin))
(in-package :util/store/elb-store)

(defclass elb-store-mixin ()
  ((elb-arn :initarg :elb-arn
            :reader elb-arn
            :initform nil)))

(defun instance-id ()
  (str:trim
   (uiop:run-program
    (list "ec2metadata" "--instance-id")
    :output 'string)))


(auto-restart:with-auto-restart (:retries 3 :sleep 1)
  (defmethod  run-target-cmd (self cmd)
    (when (elb-arn self)
      (uiop:run-program
       (list "aws" cmd "--target-group-arn"
             (elb-arn self)
             "--targets"
             (format nil "Id=~a" (instance-id)))))))

(defmethod on-leader-start ((self elb-store-mixin))
  (ignore-and-log-errors ()
   (run-target-cmd self "register-targets")))

(defmethod on-leader-stop ((self elb-store-mixin))
  (ignore-and-log-errors ()
   (run-target-cmd self "deregister-targets")))

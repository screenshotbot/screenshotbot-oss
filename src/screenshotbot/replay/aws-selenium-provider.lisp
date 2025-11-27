;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/aws-selenium-provider
  (:use #:cl)
  (:import-from #:screenshotbot/replay/services
                #:call-with-selenium-server))
(in-package :screenshotbot/replay/aws-selenium-provider)

(defclass aws-selenium-provider ()
  ((security-group :initarg :security-group
                   :reader security-group)
   (subnet-id :initarg :subnet-id
              :reader subnet-id)
   (iam-profile :initarg :iam-profile
                :reader iam-profile)
   (proxy-binary :initarg :proxy-binary
                 :reader proxy-binary)
   (ami :initarg :ami
        :initform "ami-0030d3967006e88b5"
        :reader ami)))

(defmethod call-with-selenium-server ((self aws-selenium-provider)
                                      fn &key type)
  (let ((instance-id (aws-new-instance self)))
    (log:info "Started new instance: ~a" instance-id)
    (unwind-protect
         (funcall fn instance-id)
      (aws-terminate-instance instance-id))))


(defmethod aws-new-instance ((self aws-selenium-provider))
  "Create a new AWS EC2 instance for selenium testing"
  (let* ((image-id (ami self)) 
         (instance-type "t3a.medium")
         (subnet-id (subnet-id self))
         (user-data (format nil "#!/bin/bash
aws s3 cp ~a ./proxy
curl https://screenshotbot.io/force-crash
sudo apt-get update
sudo apt-get install -y docker
" (proxy-binary self)))
         (user-data-file (format nil "/tmp/user-data-~A.sh" (get-universal-time))))
    ;; Write user-data to temporary file
    (with-open-file (stream user-data-file :direction :output :if-exists :supersede)
      (write-string user-data stream))
    (unwind-protect
        (let* ((run-result (uiop:run-program 
                            (list "aws" "ec2" "run-instances"
                                  "--image-id" image-id
                                  "--count" "1"
                                  "--instance-type" instance-type
                                  "--security-group-ids" (security-group self)
                                  "--subnet-id" subnet-id
                                  "--iam-instance-profile" (format nil "Name=~A" (iam-profile self))
                                  "--user-data" (format nil "file://~A" user-data-file)
                                  "--output" "json")
                           :error-output t
                           :output :string))
               (response (yason:parse run-result))
               (instances (gethash "Instances" response))
               (instance-id (gethash "InstanceId" (first instances))))
          (aws-wait-for-instance-running self instance-id)
          instance-id)
      ;; Cleanup temporary file
      (when (probe-file user-data-file)
        (delete-file user-data-file)))))

(defmethod aws-wait-for-instance-running ((self aws-selenium-provider) instance-id)
  "Wait for the EC2 instance to be in running state"
  (loop with max-attempts = 30
        with attempt = 0
        do (let* ((result (uiop:run-program
                          (list "aws" "ec2" "describe-instances"
                                "--instance-ids" instance-id
                                "--output" "json")
                          :output :string))
                  (response (yason:parse result))
                  (reservations (gethash "Reservations" response))
                  (instances (gethash "Instances" (first reservations)))
                  (instance (first instances))
                  (state (gethash "Name" (gethash "State" instance))))
             (when (string= state "running")
               (return t))
             (when (>= (incf attempt) max-attempts)
               (error "Instance ~A failed to start within timeout" instance-id))
             (sleep 10))))


(defmethod aws-terminate-instance (instance-id)
  "Terminate an AWS EC2 instance"
  (log:info "Terminating instance: ~a" instance-id)
  (let ((result (uiop:run-program
                 (list "aws" "ec2" "terminate-instances"
                       "--instance-ids" instance-id
                       "--output" "json")
                 :error-output t
                 :output :string)))
    (let* ((response (yason:parse result))
           (terminating-instances (gethash "TerminatingInstances" response))
           (instance (first terminating-instances))
           (current-state (gethash "Name" (gethash "CurrentState" instance))))
      (log:info "Instance ~a state: ~a" instance-id current-state)
      current-state)))


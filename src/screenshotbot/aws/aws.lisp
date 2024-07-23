(defpackage :screenshotbot/aws/aws
  (:use #:cl))
(in-package :screenshotbot/aws/aws)


(defvar *aws-profile* "AdministratorAccess-096795202767")
(defvar *vpc-id* "vpc-0cb0e16b" #| primary-vpc |#)

(defun %aws (args &key (output 'string))
  (uiop:run-program
   `("aws"
     ,@args
     "--profile" ,*aws-profile*)
   :output output
   :error-output t))

(defun aws (&rest args)
  (%aws args ))

(defvar *region* "us-east-1b")

(defun create-security-group (group)
  (aws
   "ec2" "create-security-group"
   "--group-name" group
   "--vpc-id" *vpc-id*
   "--description" (format nil "Security group for cluster ~a" group)))

(defvar *cluster* "wdb-sandbox")

(defun create-cluster (group)
  (let ((*cluster* group))
    (setf (uiop:getenv "AWS_PROFILE")  *aws-profile*)
    (create-security-group *cluster*)))

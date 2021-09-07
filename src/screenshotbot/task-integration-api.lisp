;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/task-integration-api
    (:use #:cl #:alexandria)
  (:export
   #:task-integration
   #:send-task
   #:enabledp
   #:register-task-integration
   #:task-integrations-list
   #:task-integration-company
   #:get-issue-content))
(in-package :screenshotbot/task-integration-api)

(defclass task-integration ()
  ((company :initarg :company
            :reader task-integration-company)))

(defgeneric send-task (task-integration report))

(defgeneric enabledp (task-integration))

(defgeneric get-issue-content (task-integration report))

(defvar *integrations* nil)

(defun register-task-integration (name)
  (assert (symbolp name))
  (pushnew name *integrations*))

(defun task-integrations-list ()
  (reverse *integrations*))

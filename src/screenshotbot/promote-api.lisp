;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop/package:define-package :screenshotbot/promote-api
    (:use #:cl #:alexandria)
  (:import-from #:screenshotbot/installation
                #:installation
                #:plugin
                #:plugins
                #:promoters)
  (:export
   #:maybe-send-tasks
   #:maybe-promote
   #:promoter
   #:register-promoter
   #:plugin-promoter
   #:list-promoters
   #:start-promotion-thread))
(in-package :screenshotbot/promote-api)

(defclass promoter ()
  ())

(defgeneric maybe-send-tasks (promoter run))

(defgeneric maybe-promote (promoter run))

(defvar *promoters* nil)


(defmethod plugin-promoter ((plugin plugin))
  nil)

(defun register-promoter (name)
  (assert (symbolp name))
  (pushnew name *promoters*))

(defun list-promoters ()
  (restart-case
      (remove-if #'null
                   (append
                    (loop for plugin in (plugins (installation))
                          collect (plugin-promoter plugin))
                    (mapcar #'make-instance
                              *promoters*)))
    (recreate-promoter-list ()
      (list-promoters))))

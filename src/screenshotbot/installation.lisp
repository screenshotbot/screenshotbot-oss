;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;;; Singleton state representing the current installation

(pkg:define-package :screenshotbot/installation
    (:use #:cl
          #:./plugin)
  (:export #:installation
           #:plugin
           #:find-plugin
           #:with-plugin
           #:plugins
           #:auth-provider
           #:auth-providers
           #:auth-provider-signin-form
           #:auth-provider-signup-form
           #:standard-auth-provider))

(defclass installation ()
  ((plugins :initform nil
            :initarg :plugins
            :accessor plugins)
   (auth-providers :initform (list
                              (make-instance 'standard-auth-provider))
                   :initarg :auth-providers
                   :accessor auth-providers)))

(defclass auth-provider ()
  ())

(defclass standard-auth-provider (auth-provider)
  ())

(defgeneric auth-provider-signin-form (auth-provider redirect))

(defgeneric auth-provider-signup-form (auth-provider invite-code
                                       plan
                                       redirect))


(defclass email-auth-provider ()
  ())

(defvar *installation* (make-instance 'installation))

(defun installation ()
  *installation*)

(defun (setf installation) (inst)
  (setf *installation* inst))

(defmethod find-plugin ((installation installation) plugin-class)
  (loop for x in (plugins installation)
        if (typep x plugin-class)
          return x
        finally
           (error "plugin ~a not configured" plugin-class)))

(defmacro with-plugin ((name) &body body)
  `(let ((,name (find-plugin (installation) ',name)))
     ,@body))

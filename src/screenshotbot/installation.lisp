;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;;; Singleton state representing the current installation

(pkg:define-package :screenshotbot/installation
    (:use #:cl
          #:./plugin)
  (:import-from #:./mailer
                #:noop-mailer)
  (:export #:installation
           #:plugin
           #:find-plugin
           #:with-plugin
           #:plugins
           #:mailer
           #:auth-provider
           #:auth-providers
           #:mailer*
           #:auth-provider-signin-form
           #:auth-provider-signup-form
           #:standard-auth-provider
           #:default-oidc-provider))

(defclass installation ()
  ((plugins :initform nil
            :initarg :plugins
            :accessor plugins)
   (mailer :initform (make-instance 'noop-mailer)
           :initarg :mailer
           :accessor mailer)
   (auth-providers :initform (list
                              (make-instance 'standard-auth-provider))
                   :initarg :auth-providers
                   :accessor auth-providers)
   (default-oidc-provider :initform :nil
                          :initarg :default-oidc-provider
                          :accessor default-oidc-provider)))

(defun mailer* (&optional (installation (installation)))
  (mailer installation))

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

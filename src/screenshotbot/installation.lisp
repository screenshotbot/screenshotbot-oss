;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;;; Singleton state representing the current installation

(uiop/package:define-package :screenshotbot/installation
    (:use #:cl #:screenshotbot/plugin)
  (:import-from #:screenshotbot/mailer
                #:noop-mailer)
  (:export
   #:installation
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
   #:multi-org-feature
   #:default-oidc-provider
   #:installation-domain
   #:cached-singleton-company))
(in-package :screenshotbot/installation)

(defclass installation ()
  ((plugins :initform nil
            :initarg :plugins
            :accessor plugins)
   (domain :initarg :domain
           :initform "https://screenshotbot.example.com"
           :accessor installation-domain)
   (mailer :initform (make-instance 'noop-mailer)
           :initarg :mailer
           :accessor mailer)
   (auth-providers :initform (list
                              (make-instance 'standard-auth-provider))
                   :initarg :auth-providers
                   :accessor auth-providers)
   (default-oidc-provider :initform nil
                          :initarg :default-oidc-provider
                          :accessor default-oidc-provider)
   (singleton-company :initform nil
                      :initarg :singleton-company
                      :accessor cached-singleton-company)))


(defclass multi-org-feature ()
  ()
  (:documentation "subclassing multi-org-feature, turns on the ability
  for each user to create their own organizations. Most installations
  don't really want that s so it's turned off by default. It's used on
  screenshotbot.io"))

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

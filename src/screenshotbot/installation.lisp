;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;;; Singleton state representing the current installation

(uiop/package:define-package :screenshotbot/installation
  (:use #:cl #:screenshotbot/plugin
        #:core/installation/auth-provider
        #:core/installation/auth)
  (:import-from #:core/installation/mailer
                #:noop-mailer
                #:mailer*
                #:mailer)
  (:import-from #:core/installation/installation
                #:*installation*
                #:abstract-installation
                #:installation-domain)
  (:import-from #:screenshotbot/login/common
                #:standard-auth-provider)
  (:export
   #:installation
   #:installation-domain
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
   #:default-logged-in-page
   #:installation-s3-store
   #:null-s3-store
   #:desktop-installation
   #:pre-compiled-assets
   #:one-owned-company-per-user
   #:call-with-ensure-user-prepared
   #:replay-password
   #:oss-installation))
(in-package :screenshotbot/installation)

(defclass installation (abstract-installation)
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
   (s3-store :initform (make-instance 'null-s3-store)
             :initarg :s3-store
             :accessor installation-s3-store)
   (replay-password :initarg :replay-password
                    :initform nil
                    :accessor replay-password)
   (default-oidc-provider :initform nil
                          :initarg :default-oidc-provider
                          :accessor default-oidc-provider)
   (cdn :initarg :cdn
        :initform nil
        :accessor installation-cdn)))


(defclass desktop-installation (installation)
  ((pre-compiled-assets :reader pre-compiled-assets
                        :initarg :pre-compiled-assets
                        :initform (make-hash-table))))

(defclass oss-installation (installation)
  ())

(defclass multi-org-feature ()
  ()
  (:documentation "subclassing multi-org-feature, turns on the ability
  for each user to create their own organizations. Most installations
  don't really want that s so it's turned off by default. It's used on
  screenshotbot.io"))

(defclass one-owned-company-per-user ()
  ()
  (:documentation "When added with multi-org-feature, this ensures that
every user will have only one company that they are an owner of."))

(defclass email-auth-provider ()
  ())

(defclass null-s3-store ()
  ())

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

(defgeneric default-logged-in-page (installation))

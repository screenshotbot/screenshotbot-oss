;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/config-cli
  (:use #:cl)
  (:import-from #:clingon
                #:make-command)
  (:import-from #:server/cluster/status
                #:get-pid
                #:eval-on-pid)
  (:import-from #:core/config/model
                #:config)
  (:import-from #:clingon.options
                #:make-option)
  (:import-from #:clingon.command
                #:getopt))
(in-package :server/config-cli)

(defun config/command ()
  (make-command :name "config"
                        :description "Configure your Screenshotbot installation"
                        :handler (lambda (cmd)
                                   (clingon:print-usage-and-exit cmd t))
                        :sub-commands (list
                                       (get/command)
                                       (set/command)
                                       (setup-sso/command))))


(defun get/command ()
  (make-command :name "get"
                :description "Get a config value"
                :handler #'get/handler
                :options (list
                          (make-option
                           :string
                           :description "The key to retrieve"
                           :long-name "key"
                           :key :key))))

(defun get/handler (cmd)
  (let ((key (getopt cmd :key)))
    (when (str:emptyp key)
      (error "Must provide --key"))
    (eval-on-pid (get-pid)
                 `(format t "~a~%" (config ,key)))))

(defun set/command ()
  (make-command :name "set"
                :description "Set a config value"
                :handler #'set/handler
                :options (list
                          (make-option
                           :string
                           :description "The key to set"
                           :long-name "key"
                           :key :key)
                          (make-option
                           :string
                           :description "The value to set"
                           :long-name "value"
                           :key :value))))

(defun set/handler (cmd)
  (let ((key (getopt cmd :key))
        (value (getopt cmd :value)))
    (when (str:emptyp key)
      (error "Must provide --key"))
    (unless value
      (error "Must provide --value"))

    (eval-on-pid (get-pid)
                 `(setf (config ,key) ,value))))

(defun setup-sso/command ()
  (make-command :name "setup-sso"
                :description "A helper tool to setup SSO via OpenID Connect"
                :handler #'setup-sso/handler))

(defun setup-sso/handler (cmd)
  (declare (ignore cmd))
  (let ((args))
   (labels ((read-config (config input)
              (format t "~a [~a]: " input config)
              (finish-output t)
              (let ((arg (read-line)))
                (cond
                  ((str:blankp arg)
                   (read-config config input))
                  (t
                   (push (list config (str:trim arg)) args))))))
     (read-config "sso.oidc.issuer" "Issuer URL")
     (read-config "sso.oidc.client-id" "Client ID")
     (read-config "sso.oidc.client-secret" "Client Secret")
     (loop for (key value) in args
           do
              (log:info "Setting ~a" key)
              (eval-on-pid
               (get-pid)
               `(setf (config ,key) ,value)))
     (log:info "SSO has been configured, try testing it"))))

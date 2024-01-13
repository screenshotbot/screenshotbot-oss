;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/services
  (:use #:cl)
  (:import-from #:screenshotbot/replay/proxy
                #:selenium-host
                #:selenium-port)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/installation
                #:installation
                #:replay-password)
  (:nicknames :screenshotbot/pro/replay/services)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:selenium-port
   #:selenium-host
   #:selenium-type
   #:squid-proxy
   #:linode?))
(in-package :screenshotbot/replay/services)

(named-readtables:in-readtable :interpol-syntax)

(defclass registry ()
  ())

(defvar *registry* (make-instance 'registry))

(defclass selenium-server ()
  ((host :initarg :host
         :reader selenium-host)
   (port :initarg :port
         :reader selenium-port)
   (type :initarg :type
         :reader selenium-type)
   (squid-proxy :initarg :squid-proxy
                :reader squid-proxy)
   (selenium-hub :initarg :selenium-hub)))

(defmethod selenium-server-url ((self selenium-server))
  (format nil "http://~a:~a" (selenium-host self)
          (selenium-port self)))

(defun oss? ()
  (progn #+screenshotbot-oss t))

(defun linode? ()
  (unless (oss?)
    (or
     (equal "localhost" (uiop:hostname))
     (equal "prod1.screenshotbot.io" (uiop:hostname)))))

(defun scale-provider ()
  (make-instance 'vagrant))

(defmacro def-registry-handler ((&rest args1) (&rest args2) &body body)
  `(defhandler ,args1 ,args2
     (multiple-value-bind (user pass) (hunchentoot:authorization)
       (assert (equal user "replay"))
       (assert (equal pass (replay-password (installation)))))
     ,@body))

(def-registry-handler (nil :uri "/replay/register" :method :put) ()
  (log:info "Dummy replay register method"))

(defun selenium-server (&key (type (error "specify type")))
  (assert (member type '("firefox" "chrome" #-screenshotbot-oss "safari") :test #'equal))
  (make-instance 'selenium-server
                  :host (cond
                          ((oss?)
                           "selenium-hub")
                          ((linode?)
                           "172.30.1.180")
                          (t
                           "172.17.0.1"))
                  :squid-proxy (cond
                                 #-screenshotbot-oss
                                 ((equal "safari" type)
                                  (cond
                                    ((equal "thecharmer" (uiop:hostname))
                                     "172.17.0.1:3129")
                                    (t
                                     "192.168.1.120:3128")))
                                 ((equal "thecharmer" (uiop:hostname))
                                  "172.17.0.1:3129")
                                 (t
                                  ;; docker
                                  "squid:3128"))
                  :port (cond
                         ((or (oss?))
                          4444)
                         (t
                          5004))
                  :type nil))


(defun call-with-selenium-server (fn &key type)
  (funcall fn
           (selenium-server :type type)))

(defmacro with-selenium-server ((var &rest args) &body body)
  `(call-with-selenium-server
    (lambda (,var) ,@body)
    ,@args))

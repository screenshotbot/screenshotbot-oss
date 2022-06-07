;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/services
  (:use #:cl)
  (:nicknames :screenshotbot/pro/replay/services)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:selenium-port
   #:selenium-host
   #:selenium-type
   #:squid-proxy))
(in-package :screenshotbot/replay/services)

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
  (equal "localhost" (uiop:hostname)))

(defun selenium-server (&key (type (error "specify type")))
  (assert (member type '("firefox" "chrome" #-screenshotbot-oss "safari") :test #'equal))
  (make-instance 'selenium-server
                  :host (cond
                          ((oss?)
                           "selenium-hub")
                          ((linode?)
                           "10.9.8.2")
                          (t
                           "selenium-hub"))
                  :squid-proxy (cond
                                 #-screenshotbot-oss
                                 ((equal "safari" type)
                                  (cond
                                    ((equal "thecharmer" (uiop:hostname))
                                     "192.168.1.119:3128")
                                    (t
                                     "192.168.1.120:3128")))
                                 (t
                                  ;; docker
                                  "squid:3128"))
                  :port 4444
                  :type nil))

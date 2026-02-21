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
  (:import-from #:core/installation/installation
                #:*installation*)
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
  (let ((host (selenium-host self)))
    (format nil "http://~a:~a"
            ;; IPv6 addresses contain colons and must be wrapped in brackets
            (if (find #\: host)
                (format nil "[~a]" host)
                host)
            (selenium-port self))))

(defun selenium-server (&key (type (error "specify type")))
  (assert (member type '("firefox" "chrome") :test #'equal))
  (make-instance 'selenium-server
                 ;; This is the IP of the replay server.
                 :host "172.30.1.180"
                 ;; This is a docker instance that is running on the the replay server.
                 :squid-proxy "squid:3128"
                 :port 5004
                 :type nil))


(defclass static-selenium-provider ()
  ()
  (:documentation "An older selenium provider"))

(defmethod selenium-provider (installation)
  (make-instance 'static-selenium-provider))

(defmethod call-with-selenium-server ((self static-selenium-provider)
                                      fn &key type)
  (funcall fn
           (selenium-server :type type)))

(defmacro with-selenium-server ((var &rest args) &body body)
  `(call-with-selenium-server
    (selenium-provider *installation*)
    (lambda (,var) ,@body)
    ,@args))

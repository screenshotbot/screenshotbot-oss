;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/services
  (:use #:cl)
  (:import-from #:scale/vagrant
                #:vagrant)
  (:import-from #:scale/core
                #:ssh-sudo
                #:add-url
                #:ssh-run
                #:with-instance)
  (:import-from #:scale/linode
                #:linode)
  (:import-from #:scale/image
                #:image-spec-serialized-key
                #:with-imaged-instance
                #:defimage)
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
     (str:starts-with-p "ip-" (uiop:hostname)))))

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

(defun install-firefox (instance version)
  ;; See https://github.com/browser-actions/setup-firefox/blob/master/src/DownloadURL.ts
  (ssh-sudo instance (list "apt-get" "install" "-y" "libasound2"
                           ;; just install all the damn deps
                           "firefox-esr"
                           ;; Required to uncompress the downloaded
                           ;; file, at least on the Linode image.
                           "bzip2"))
  (add-url
   instance
   (format nil
           "https://ftp.mozilla.org/pub/firefox/releases/~a/linux-x86_64/en-US/firefox-~a.tar.bz2" version version)
   "firefox.tar.bz2")
  (ssh-run instance "ls -l")
  (ssh-run instance "file firefox.tar.bz2")
  (ssh-run
   instance
   "tar xvjf firefox.tar.bz2")
  (ssh-run instance "ls -l")
  (ssh-run instance "firefox/firefox --version"))

(defun install-geckodriver (instance)
  (add-url
   instance
   "https://github.com/mozilla/geckodriver/releases/download/v0.31.0/geckodriver-v0.31.0-linux64.tar.gz"
   "geckodriver.tar.gz")

  (ssh-run
   instance
   "tar xvzf geckodriver.tar.gz"))

(defimage (debian-base :instance instance)
          ()
          :debian-11
  (ssh-sudo instance (list "apt-get" "update"))
  (ssh-sudo instance "apt-get install -y xvfb"))

(defimage (firefox :instance instance :version 3)
    (version)
    debian-base
  (install-firefox instance version)
  (install-geckodriver instance))

(defun call-firefox-using-scale-provider (fn provider)
  (with-imaged-instance (machine (firefox :version "102.0") provider :size :small)
    ;;(ssh-run machine "./geckodriver --binary firefox/firefox")
    (funcall fn)))

#+nil
(call-firefox-using-scale-provider
 (lambda () (log:info "with image!")) (make-instance 'vagrant))

(defun call-with-selenium-server (fn &key type)
  (cond
    #+nil
    ((and (not (linode?))
          (equal "firefox" type))
     (call-firefox-using-scale-provider fn
                                        (scale-provider)))
    (t
     (funcall fn
              (selenium-server :type type)))))

(defmacro with-selenium-server ((var &rest args) &body body)
  `(call-with-selenium-server
    (lambda (,var) ,@body)
    ,@args))

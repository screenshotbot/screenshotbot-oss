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
                #:add-url
                #:ssh-run
                #:with-instance)
  (:import-from #:scale/linode
                #:linode)
  (:import-from #:scale/image
                #:with-imaged-instance
                #:defimage)
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
  (format nil "http://~a:~a" (selenium-host self)
          (selenium-port self)))

(defun oss? ()
  (progn #+screenshotbot-oss t))

(defun linode? ()
  (unless (oss?)
   (equal "localhost" (uiop:hostname))))

(defun scale-provider ()
  (make-instance 'vagrant))

(defun selenium-server (&key (type (error "specify type")))
  (assert (member type '("firefox" "chrome" #-screenshotbot-oss "safari") :test #'equal))
  (make-instance 'selenium-server
                  :host (cond
                          ((oss?)
                           "selenium-hub")
                          ((linode?)
                           "10.9.8.2")
                          (t
                           "172.17.0.1"))
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
                  :port (cond
                         ((or (oss?))
                          4444)
                         (t
                          5004))
                  :type nil))

(defun install-firefox (instance version)
  ;; See https://github.com/browser-actions/setup-firefox/blob/master/src/DownloadURL.ts
  (ssh-run instance (list "apt-get" "install" "-y" "libasound2"
                          ;; just install all the damn deps
                          "firefox-esr"))
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
  (ssh-run instance "apt-get install -y wget")
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
  (ssh-run instance (list "apt-get" "update")))

(defimage (firefox :instance instance)
    (version)
    debian-base
  (install-firefox instance version)
  (install-geckodriver instance))

(defun call-firefox-using-scale-provider (fn provider)
  (with-imaged-instance (machine (firefox :version "102.0") provider :size :small)
    (funcall fn)))

#+nil
(call-firefox-using-scale-provider (lambda () (log:info "with image!")) (make-instance 'vagrant))

(defun call-with-selenium-server (fn &key type)
  (cond
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

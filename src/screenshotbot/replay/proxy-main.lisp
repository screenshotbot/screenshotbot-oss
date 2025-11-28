;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/proxy-main
  (:use #:cl)
  (:import-from #:screenshotbot/replay/proxy
                #:replay-proxy
                #:*proxy-port*)
  (:import-from #:clingon
                #:make-option)
  (:export
   #:proxy-main
   #:proxy-main/command))
(in-package :screenshotbot/replay/proxy-main)

(defun prepare-machine ()
  "Prepare the machine by updating packages and installing Docker"
  (log:info "Preparing machine: running apt-get update")
  (uiop:run-program '("apt-get" "update")
                    :output :interactive
                    :error-output :interactive)
  (log:info "Installing Docker")
  (uiop:run-program '("apt-get" "install" "-y" "docker.io")
                    :output :interactive
                    :error-output :interactive)
  (log:info "Machine preparation complete"))

(defun proxy-main/handler (cmd)
  (let ((port (clingon:getopt cmd :port))
        (address (clingon:getopt cmd :address))
        (listen-backlog (clingon:getopt cmd :listen-backlog))
        (prepare-machine-p (clingon:getopt cmd :prepare-machine)))
    (when prepare-machine-p
      (prepare-machine))
    (server:main :acceptor (make-instance 'replay-proxy
                                          :port port
                                          :address address
                                          :listen-backlog listen-backlog)
                 :enable-store nil
                 :jvm nil)))

(defun proxy-main/command ()
  (clingon:make-command
   :name "proxy"
   :description "Screenshotbot Selenium Proxy Server"
   :handler #'proxy-main/handler
   :options (list
             (make-option
              :integer
              :description "HTTP access port"
              :long-name "port"
              :initial-value 5004
              :key :port)
             (make-option
              :string
              :description "Address to bind to"
              :long-name "address"
              :initial-value "0.0.0.0"
              :key :address)
             (make-option
              :integer
              :description "Listen backlog size"
              :long-name "listen-backlog"
              :initial-value 500
              :key :listen-backlog)
             (make-option
              :flag
              :description "Prepare machine by running apt-get update and installing Docker"
              :long-name "prepare-machine"
              :initial-value nil
              :key :prepare-machine))))

(defun proxy-main ()
  (let ((args (cdr (uiop:raw-command-line-arguments))))
    (clingon:run (proxy-main/command) args)))

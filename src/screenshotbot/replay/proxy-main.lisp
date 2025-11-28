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

(defun setup-squid ()
  "Configure and start Squid proxy"
  (log:info "Configuring Squid")

  ;; Create allowed_sites file
  (with-open-file (stream "/etc/squid/allowed_sites"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "staging~%")
    (format stream "replay~%")
    (format stream "~%")
    (format stream "# static IP for staging~%")
    (format stream "172.29.1.15~%")
    (format stream "~%")
    (format stream "# second static IP for staging~%")
    (format stream "172.20.0.6~%")
    (format stream "~%")
    (format stream "# static IP for replay~%")
    (format stream "172.29.1.14~%")
    (format stream "~%")
    (format stream "# production over OpenVPN~%")
    (format stream "10.9.8.1~%")
    (format stream "~%")
    (format stream "# debian-us-east-001~%")
    (format stream "45.33.86.76~%"))

  ;; Create squid.conf
  (with-open-file (stream "/etc/squid/squid.conf"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "http_port 3128~%")
    (format stream "cache allow all~%")
    (format stream "cache_dir aufs /var/spool/squid 50000 16 256~%")
    (format stream "acl whitelist dstdomain \"/etc/squid/allowed_sites\"~%")
    (format stream "http_access deny !whitelist~%"))

  ;; Initialize cache directories
  (log:info "Initializing Squid cache directories")
  (uiop:run-program '("squid" "-z")
                    :output :interactive
                    :error-output :interactive
                    :ignore-error-status t)

  ;; Start Squid
  (log:info "Starting Squid service")
  (uiop:run-program '("systemctl" "start" "squid")
                    :output :interactive
                    :error-output :interactive)
  (uiop:run-program '("systemctl" "enable" "squid")
                    :output :interactive
                    :error-output :interactive))

(defun prepare-machine ()
  "Prepare the machine by updating packages and installing Docker and Squid"
  (log:info "Preparing machine: running apt-get update")
  (uiop:run-program '("apt-get" "update")
                    :output :interactive
                    :error-output :interactive)

  (log:info "Installing Docker and Squid")
  (uiop:run-program '("apt-get" "install" "-y" "docker.io" "squid")
                    :output :interactive
                    :error-output :interactive)

  (setup-squid)

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

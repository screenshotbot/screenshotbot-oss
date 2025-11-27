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

(defun proxy-main/handler (cmd)
  (let ((port (clingon:getopt cmd :port))
        (address (clingon:getopt cmd :address))
        (listen-backlog (clingon:getopt cmd :listen-backlog)))
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
              :key :listen-backlog))))

(defun proxy-main ()
  (let ((args (cdr (uiop:raw-command-line-arguments))))
    (clingon:run (proxy-main/command) args)))

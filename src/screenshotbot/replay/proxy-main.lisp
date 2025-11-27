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
  (:export
   #:proxy-main))
(in-package :screenshotbot/replay/proxy-main)

(defun proxy-main ()
  (server:main :acceptor (make-instance 'replay-proxy
                                        :port *proxy-port*
                                        :address "0.0.0.0"
                                        :listen-backlog 500)
               :enable-store nil
               :jvm nil))

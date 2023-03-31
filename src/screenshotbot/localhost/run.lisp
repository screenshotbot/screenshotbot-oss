;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/localhost/run
  (:use #:cl)
  (:import-from #:server
                #:with-cron
                #:with-lparallel-kernel)
  (:export
   #:command))
(in-package :screenshotbot/localhost/run)

(defun handler (cmd)
  (with-lparallel-kernel (:threads 4)
    (with-cron ()
      (let ((port (clingon:getopt cmd :port)))
        (let ((acceptor (make-instance 'screenshotbot/server:acceptor
                                       :port port)))
          (screenshotbot/server::prepare-acceptor-plugins acceptor)
          (hunchentoot:start acceptor)
          (log:info "The server is ready on http://localhost:~a" port)
          (loop (sleep 60)))))))

(defun options ()
  (list
   (clingon:make-option
    :integer
    :description "The port to host on, defaults to 4095"
    :short-name #\p
    :long-name "port"
    :initial-value "4095"
    :key :port)))

(defun command ()
  (clingon:make-command :name "run"
                        :description "Run the screenshotbot service"
                        :options (options)
                        :handler #'handler))

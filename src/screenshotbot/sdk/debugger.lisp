;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; Only part of the debug CLI, see deliver-debug-sdk.lisp.

(defpackage :screenshotbot/sdk/debugger
  (:use #:cl)
  (:export
   #:main
   #:maybe-start-remote-debugging))
(in-package :screenshotbot/sdk/debugger)

(defvar *debug-port* 2001
  "The port on $SCREENSHOTBOT_DEBUG_HOST that the LispWorks IDE listens
on for remote debugging connections.")

(defun maybe-start-remote-debugging ()
  "If $SCREENSHOTBOT_DEBUG_HOST is set, open a remote debugging
connection to that host, and then never return: the CLI only exists to
be driven from the IDE at the other end.

log4cl isn't configured this early, hence the FORMATs."
  #+lispworks
  (let ((host (uiop:getenv "SCREENSHOTBOT_DEBUG_HOST")))
    (when (str:non-empty-string-p host)
      (format t "Connecting to remote debugger on ~a:~a~%" host *debug-port*)
      (finish-output)
      (let ((stream (comm:open-tcp-stream host *debug-port*
                                          :direction :io
                                          :errorp t
                                          :keepalive t
                                          :read-timeout nil)))
        ;; The connection gets its own process.
        (dbg:create-client-remote-debugging-connection
         "screenshotbot-cli"
         :stream stream))
      (format t "Remote debugging connection is up, sleeping forever~%")
      (finish-output)
      (loop (sleep 60)))))

(defun main (&rest args)
  "The entry point for the debug CLI. Other than the remote debugging,
this behaves exactly like the CLI we ship."
  (maybe-start-remote-debugging)
  (apply #'screenshotbot/sdk/main:main args))

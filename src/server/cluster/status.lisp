;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/cluster/status
  (:use #:cl)
  (:import-from #:clingon
                #:getopt
                #:make-option)
  #+lispworks
  (:import-from #:unix-sockets
                #:connect-unix-socket
                #:unix-socket-stream)
  (:export #:cluster-status/command
           #:eval-on-pid
           #:get-pid))
(in-package :server/cluster/status)

(defvar *service-name* "screenshotbot"
  "The systemd service name for the screenshotbot server.")

(defun get-pid ()
  "Get the PID of the running screenshotbot service from systemd."
  (let* ((output (uiop:run-program
                  (list "systemctl" "show" "--property" "MainPID" "--value" *service-name*)
                  :output :string))
         (pid-string (string-trim '(#\Space #\Newline #\Return) output)))
    (let ((pid (parse-integer pid-string :junk-allowed t)))
      (when (and pid (> pid 0))
        pid))))

(defun sockets-dir ()
  "Return the directory where control sockets are stored."
  "/home/arnold/sockets/")

(defun get-socket-file (pid)
  "Return the socket file path for a given PID."
  (path:catfile (sockets-dir) (format nil "~a.sock" pid)))

#+lispworks
(defun eval-on-pid (pid expr)
  "Evaluate EXPR on the server running with the given PID via its control socket.
Returns the result of the evaluation."
  (let* ((socket-file (get-socket-file pid))
         (socket (connect-unix-socket (namestring socket-file))))
    (unwind-protect
         (let ((conn (dbg:create-ide-remote-debugging-connection
                      "ClusterStatus"
                      :stream (unix-socket-stream socket))))
           (unwind-protect
                (multiple-value-bind (result status)
                    (dbg:ide-eval-form-in-remote
                     expr
                     :output-stream *standard-output*
                     :timeout 30
                     :connection conn)
                  (format t "Got result: ~a, ~a~%" result status)
                  (values result status))
             (dbg:close-remote-debugging-connection conn)))
      (close (unix-socket-stream socket)))))

#-lispworks
(defun eval-on-pid (pid expr)
  (declare (ignore pid expr))
  (error "eval-on-pid is only supported on LispWorks"))

(defun cluster-status/command ()
  (clingon:make-command :name "status"
                        :description "Show cluster status"
                        :handler #'cluster-status/handler
                        :options nil))

(defun cluster-status/handler (cmd)
  (declare (ignore cmd))
  (let ((pid (get-pid)))
    (cond
      (pid
       (show-status-for-pid pid))
      (t
       (format t "No running server found (service: ~a)~%" *service-name*))))
  (values))

(defun show-status-for-pid (pid)
  "Show the status of the server with the given PID."
  #+lispworks
  (handler-case
      (progn
        (format t "Server PID: ~a~%" pid)
        (eval-on-pid pid
                     '(let ((store bknr.datastore:*store*))
                       (format t "Leader: ~a~%"
                        (bknr.cluster/server:leader-id store))
                       (format t "Is current node Leader?: ~a~%"
                        (bknr.cluster/server:leaderp store))
                       (format t "Peers:~%")
                       (loop for peer in (bknr.cluster/server:list-peers store)
                             do (format t "  ~a~%" peer)))))
    (error (e)
      (format t "Error connecting to PID ~a: ~a~%" pid e)))
  #-lispworks
  (format t "PID: ~a (status checking only supported on LispWorks)~%" pid))

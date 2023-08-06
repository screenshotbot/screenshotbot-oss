;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/eval
  (:use #:cl)
  (:import-from #:clingon
                #:getopt
                #:make-option)
  (:import-from #:unix-sockets
                #:with-unix-socket)
  (:export
   #:eval/command))
(in-package :server/eval)

(defun eval/command ()
  (clingon:make-command
   :name "eval"
   :handler (lambda (cmd)
              (let ((socket (getopt cmd :socket))
                    (expr (getopt cmd :expr)))
                (perform-eval socket expr)))
   :options (list
             (make-option
              :string
              :description "The Unix domain socket"
              :long-name "socket"
              :key :socket)
             (make-option
              :string
              :description "The Lisp expression"
              :long-name "expression"
              :key :expr))))

(defun perform-eval (socket expr)
  (log:info "Opening socket connection")
  (with-unix-socket (socket (unix-sockets:connect-unix-socket socket))
    (log:info "Creating debugging connection")
    (let ((conn (dbg:create-ide-remote-debugging-connection
                 "Eval"
                 :stream (unix-sockets:unix-socket-stream socket))))
      (unwind-protect
           (progn
             (log:info "Evaluating expression")
             (let ((status (dbg:ide-eval-form-in-remote
                            `(block nil
                               (handler-bind ((error (lambda (e)
                                                       (format t "~a" e)
                                                       (dbg:output-backtrace :brief)
                                                       (with-open-file (file "log/eval-crash-log" :direction :output :if-exists :supersede)
                                                         (dbg:output-backtrace :verbose file))
                                                       (return-from nil nil))))
                                 ,(read-from-string expr)
                                 (sleep 2)
                                 t))
                            :output-stream *standard-output*
                            :connection conn)))
               (cond
                 ((not status)
                  (log:info "Command failed: ~a" expr)
                  (uiop:quit 1))
                 (t
                  (log:info "Command ran without errors")))))
        (dbg:close-remote-debugging-connection conn)))))

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
  (:import-from #:util/misc
                #:safe-with-open-file)
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

(defun safely-eval (expr tmp)
  (format *standard-output* "Using tmpfile: ~s" tmp)
  (safe-with-open-file (file tmp :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
    (block nil
      (format *standard-output* "Inside block~%")
      (handler-bind ((error (lambda (e)
                              (declare (ignore e))
                              (format t "Got error: ~a~%" e)
                              (dbg:output-backtrace :brief *standard-output*)
                              (return-from nil :fail))))
        (eval (read-from-string expr))
        (format *standard-output* "End of block~%")
        :good))))

(defun perform-eval (socket expr)
  (log:info "Opening socket connection v2.")
  (let ((socket
          ;; NOTE: dbg:create-ide-remote-debugging-connection requires
          ;; us to NOT close the socket/stream.
          (unix-sockets:connect-unix-socket socket)))
    (log:info "Creating debugging connection")
    (let ((conn (dbg:create-ide-remote-debugging-connection
                 "Eval"
                 :stream (unix-sockets:unix-socket-stream socket))))
      (unwind-protect
           (progn
             (log:info "Evaluating expression")
             (uiop:with-temporary-file (:pathname tmp :keep t :prefix "deploy-eval")
               (delete-file tmp)
               (log:info "Logging output to: ~a" tmp)
               (let ((final-expr
                       `(safely-eval ,expr ,(namestring tmp))))
                 (log:info "Full expression ~S" final-expr)
                 (multiple-value-bind (status reason)
                     (dbg:ide-eval-form-in-remote
                      final-expr
                      :output-stream *standard-output*
                      :timeout 1200
                      :connection conn)
                   (ignore-errors
                    (write-string (uiop:read-file-string tmp)))
                   (case status
                     (:fail
                      (log:info "Command failed: ~a" expr)
                      (uiop:quit 1))
                     (:good
                      (delete-file tmp)
                      (log:info "Command ran without errors"))
                     (otherwise
                      (log:info "Command failed with ~a: ~a [~a]" status expr reason)
                      (uiop:quit 1)))))))
        (dbg:close-remote-debugging-connection conn)))))

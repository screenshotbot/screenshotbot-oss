;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/desktop/init
  (:use #:cl)
  (:export
   #:main)
  (:local-nicknames (#:run #:screenshotbot/desktop/run)
                    (#:self-test #:screenshotbot/desktop/self-test)))
(in-package :screenshotbot/desktop/init)

(defun main/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun main/command ()
  (clingon:make-command :name "Screenshotbot Local"
                        :version "0.1.0"
                        :description "Access Screenshotbots image comparison tools locally without internet access."
                        :authors (list "Modern Interpreters Inc")
                        :license "MPLv2"
                        :handler #'main/handler
                        :sub-commands (list (run:command)
                                            (self-test:command))))
(defun main ()
  (handler-bind ((error (lambda (e)
                          (declare (ignore e))
                          #+lispworks
                          (dbg:output-backtrace :brief ))))
    (uiop:setup-command-line-arguments)
    (let ((args (cdr (uiop:raw-command-line-arguments))))
      (let ((app (main/command)))
        (clingon:run app args)))))

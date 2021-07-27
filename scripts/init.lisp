;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; This file is loaded after the image is restarted

(defpackage #:tdrhq-init
  (:use #:cl
        #:cl-user))

(in-package #:tdrhq-init)

(ql:quickload :deadbeef)

(defun register-tdrhq (name)
  (deadbeef:register-external (format nil "https://github.com/tdrhq/~a" name)
                              "master"))

(deadbeef:register-external "https://github.com/moderninterpreters/markup"
                            "master")

(register-tdrhq "hunchentoot")
(register-tdrhq "stripe")
(register-tdrhq "fiveam")
(register-tdrhq "cl-mongo-id")
(register-tdrhq "named-readtables")
(register-tdrhq "cmd")

(deadbeef:register-external "https://github.com/cl-plus-ssl/cl-plus-ssl"
                            "master")

(deadbeef:register-external "https://github.com/gschjetne/cljwt"
                            "master")

(deadbeef:register-external "https://github.com/moderninterpreters/clsql-local-time"
                            "master")


(deadbeef:prepare-externals "build/deadbeef/")

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

(ql:quickload :quick-patch)

(defun register-tdrhq (name commit)
  (quick-patch:register-external (format nil "https://github.com/tdrhq/~a" name)
                              commit))

(quick-patch:register-external "https://github.com/moderninterpreters/markup"
                            "5357f8a980cd5d884468734d7511323d00844175")

(register-tdrhq "hunchentoot" "ade1b9bb1c110a180cb883aed105536cbb0ea5e7")
(register-tdrhq "stripe" "6b91ee9bcbffe81f887a0edddd1b182951cd02cf")
(register-tdrhq "fiveam" "162bd30e6179fc787ee94e96104c8ce059d610ff")
(register-tdrhq "cl-mongo-id" "5313c5f8b5cc035818372681297d75966ecf1d93")
(register-tdrhq "named-readtables" "6ae08604e907959e33d3a19f1f1ae0733adc0dcd")
(register-tdrhq "cmd"  "29f1267d141b5117dc742bce74340711c99076f3")

(quick-patch:register-external "https://github.com/tdrhq/cl-plus-ssl"
                            "6ed1f7e916346d96c98dbcd25493c1b206b4e44e")

(quick-patch:register-external "https://github.com/gschjetne/cljwt"
                            "bd3e567097cd9d48eb811be601590afa167e6667")

(quick-patch:register-external "https://github.com/moderninterpreters/clsql-local-time"
                            "3a6d1f93cbe1549edc3ece63ed473d1dbd31c241")

(quick-patch:prepare-externals "build/quick-patch/")

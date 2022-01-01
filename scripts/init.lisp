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
  (quick-patch:register (format nil "https://github.com/tdrhq/~a" name)
                              commit))

(quick-patch:register "https://github.com/moderninterpreters/markup"
                            "4dd66b0a0223f58e581697a1713ffbf77c0ab798")


(register-tdrhq "stripe" "673d4b9600eb7c2dd21b4701a1b18e348dca7267")

(register-tdrhq "fiveam" "162bd30e6179fc787ee94e96104c8ce059d610ff")
(register-tdrhq "cl-mongo-id" "5313c5f8b5cc035818372681297d75966ecf1d93")
(register-tdrhq "named-readtables" "6ae08604e907959e33d3a19f1f1ae0733adc0dcd")
(register-tdrhq "cmd"  "29f1267d141b5117dc742bce74340711c99076f3")
(register-tdrhq "cl-elastic" "0607087887da76919219b01d0601027bb198b444")
(register-tdrhq "opticl" "a33e3411d28ebff4b29a59a3619884c0f54ff586")
(register-tdrhq "cl-webdriver-client" "627869224aabc118ca2db8a802ef177fb24a41ec")

(quick-patch:register "https://github.com/tdrhq/cl-plus-ssl"
                            "306b17bbae07a01fb32ecb50f83ed1bd9cbed04e")

(quick-patch:register "https://github.com/gschjetne/cljwt"
                            "bd3e567097cd9d48eb811be601590afa167e6667")

(quick-patch:register "https://github.com/moderninterpreters/clsql-local-time"
                      "3a6d1f93cbe1549edc3ece63ed473d1dbd31c241")

(quick-patch:register "https://github.com/tokenrove/imago"
                      "29f2b42b248785acae3d05d5dd97a4e9ad0d8ecb")

(quick-patch:checkout-all "build/quick-patch/")

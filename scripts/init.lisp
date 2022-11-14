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

(compile 'register-tdrhq)

#+nil
(register-tdrhq "stripe" "673d4b9600eb7c2dd21b4701a1b18e348dca7267")

;; Only on Windows, we're unable to checkout nyaml, because its test
;; code has file names that don't load on Windows. This patched
;; version has all the test code deleted.
#+mswindows
(register-tdrhq "nyaml" "28d43dae676ad013affeab3c2b0f0d9307490d53")

(register-tdrhq "fiveam" "162bd30e6179fc787ee94e96104c8ce059d610ff")
(register-tdrhq "cl-mongo-id" "5313c5f8b5cc035818372681297d75966ecf1d93")
(register-tdrhq "cmd"  "29f1267d141b5117dc742bce74340711c99076f3")
(register-tdrhq "opticl" "a33e3411d28ebff4b29a59a3619884c0f54ff586")

(quick-patch:register "https://github.com/tdrhq/cl-plus-ssl"
                            "306b17bbae07a01fb32ecb50f83ed1bd9cbed04e")

(quick-patch:register "https://github.com/gschjetne/cljwt"
                            "bd3e567097cd9d48eb811be601590afa167e6667")

(quick-patch:register "https://github.com/tokenrove/imago"
                      "29f2b42b248785acae3d05d5dd97a4e9ad0d8ecb")

(register-tdrhq "plump" "aeea283021da94e9d30025f79c914b37fc522b75")

;; html2text is not part of quicklisp
(register-tdrhq "html2text"
                "b5620fdd435df5254a713f3c10bd756632df3dce")

;; TODO: automatically generate hashes
#+screenshotbot-oss
(progn
  (quick-patch:register "https://github.com/moderninterpreters/markup" "master")
  (register-tdrhq "easy-macros" "main")
  (register-tdrhq "fiveam-matchers" "master"))

(quick-patch:checkout-all "build/quick-patch/")

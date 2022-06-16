;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-digests
  (:use #:cl
        #:fiveam
        #:util/digests)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-digests)


(util/fiveam:def-suite)

(defvar *fixture* (asdf:system-relative-pathname :util "tests/rose.png"))

(test sha256
  (is (equalp #(196 177 210 34 213 217 70 217 240 191 165
                243 62 242 252 170 162 253 198 228 161 32 225
                213 222 199 176 176 220 223 116 116)
              (sha256-file *fixture*))))

(test md5
  (is (equalp #(145 139 141 103 193 215 81 218 8 73 97 248 57 178 154 224)
              (md5-file *fixture*))))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-fake-fli
  (:use #:cl
        #:fiveam)
  (:local-nicknames (#:fake-fli #:util/fake-fli)))
(in-package :util/tests/test-fake-fli)

(util/fiveam:def-suite)

(fake-fli:define-foreign-function (strlen-v2 "strlen")
    ((s :lisp-simple-1d-array))
  :result-type :size-t)

(fake-fli:define-foreign-function strlen
    ((s (:reference-pass :ef-mb-string)))
  :result-type :size-t)

(test simple-foreign-function
  (is (equal 6 (strlen "foobar"))))

(Test lisp-simple-1d-array
  (let ((arr (make-array 10
                         :element-type '(unsigned-byte 8)
                         :initial-contents #(2 3 4 5 5 6 0 1 1 1))))
    (is (equal 6 (strlen-v2 arr)))))




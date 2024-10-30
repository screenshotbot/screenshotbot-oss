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


(fake-fli:define-foreign-function strlen
    ((s (:reference-pass :ef-mb-string)))
  :result-type :size-t)

(fake-fli:define-foreign-function (strlen-v2 "strlen")
    ((s :lisp-simple-1d-array))
  :result-type :size-t)

(fake-fli:define-foreign-function memcpy
    ((dest :lisp-simple-1d-array)
     (src :lisp-simple-1d-array))
  :result-type :void)

(test simple-foreign-function
  (is (equal 6 (strlen "foobar"))))

(Test lisp-simple-1d-array
  (let ((arr (make-array 10
                         :element-type '(unsigned-byte 8)
                         :initial-contents #(2 3 4 5 5 6 0 1 1 1))))
    (dotimes (i 10)
      (is (equal 6 (strlen-v2 arr))))))

(test modify-lisp-simple-1d-array
  #+nil
  (let ((arr (make-array 10
                         :element-type '(unsigned-byte 8)
                         :initial-contents #(2 3 4 5 5 6 0 1 1 1)))
        (arr2 (make-array 10
                          :element-type '(unsigned-byte 8)
                          :initial-element 9)))
    (memcpy arr2 arr)
    (is (equalp #(2 3 4 5 6 6 0 9 9 9)
                arr2))))




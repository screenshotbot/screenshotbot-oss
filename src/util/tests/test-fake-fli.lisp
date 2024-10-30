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


(test simple-foreign-function
  (is (equal 6 (strlen "foobar"))))


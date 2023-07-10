;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/posix
  (:use #:cl)
  (:local-nicknames #-lispworks
                    (#:fli #:util/fake-fli))
  (:export
   #:getpid
   #:errno
   #:get-errno-string))
(in-package :util/posix)

(fli:define-foreign-function getpid
    ()
  :result-type :int)

(defun errno ()
  #+lispworks
  (lw:errno-value)
  #+sbcl
  (sb-alien:get-errno)
  #-(:or :lispworks :sbcl)
  0)

(defun get-errno-string (errno)
  #+lispworks
  (lw:get-unix-error errno)
  #-lispworks
  "get-errno-string unsupported on this platform")

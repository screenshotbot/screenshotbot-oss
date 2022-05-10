;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/threading
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:call-with-thread-fixes
   #:make-thread))
(in-package :util/threading)


(defun call-with-thread-fixes (fn)
  (funcall fn))

(defun make-thread (body &rest args)
  (apply #'bt:make-thread
         body
         args))

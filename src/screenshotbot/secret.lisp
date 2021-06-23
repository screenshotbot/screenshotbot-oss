;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/secret
    (:use #:cl
          #:alexandria)
  (:export #:defsecret
           #:secret))

(defvar *secret-list* nil)
(defvar *secrets* nil)

(defmacro defsecret (name documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew ',name *secret-list*)))

(defun secret (name)
  (get name '%secret))

(defun (setf secret) (value name)
  (unless (member name *secret-list*)
    (error "Secret ~S is not defined" name))
  (setf
   (get name '%secret)
   value))

(define-compiler-macro secret (&whole whole name)
  (unless (member name *secret-list*)
    (error "Secret ~S is not defined" name))
  whole)

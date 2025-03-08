;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/secret
    (:use #:cl
          #:alexandria)
    (:import-from #:bknr.impex
                  #:parse-xml-file)
    (:import-from #:bknr.indices
                  #:indexed-class
                  #:base-indexed-object)
  (:export #:defsecret
           #:secret))
(in-package :screenshotbot/secret)

(defvar *secret-list* nil)
(defvar *secrets* nil)

(defclass secret (base-indexed-object)
  ((name :initarg :name
         :reader secret-name)
   (value :initarg :value
          :reader secret-value)
   (public :initarg :public
           :initform nil
           :reader secret-public-id)
   (environment :initarg :environment
                :initform "production"
                :reader secret-environment))
  (:metaclass indexed-class))

(defmethod print-object ((self secret) out)
  (format out "#<SECRET ~a [~a]>" (secret-name self)
          (secret-environment self)))

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

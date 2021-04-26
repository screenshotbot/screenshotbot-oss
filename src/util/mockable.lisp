;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage #:mockable
  (:use #:cl)
  (:export #:defmockable
	   #:with-single-mock
	   #:d-with-single-mock))
(in-package #:mockable)

(defvar *enable-lookups* nil)

(defun find-mock (sym)
  (cdr (assoc sym *enable-lookups*)))

(defmacro defmockable (name (&rest lambda-list) &body body)
  `(defun ,name (&rest defmockable-args)
     (if (and mockable::*enable-lookups*
	      (mockable::find-mock ',name))
	 (apply (find-mock ',name)
		defmockable-args)
	 (progn
	   (destructuring-bind ,lambda-list defmockable-args
	     ,@body)))))

(defmacro with-single-mock ((name value) &body body)
  `(progn
     (let ((mockable::*enable-lookups*
	    (acons ',name ,value mockable::*enable-lookups*)))
       ,@body)))

(defmacro d-with-single-mock ((name value) &body body)
  `(progn ,@body))

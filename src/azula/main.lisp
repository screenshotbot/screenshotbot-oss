;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :azula/main
  (:use :cl)
  (:export :config))
(in-package :azula/main)

(defclass build-file ()
  ((path :initarg :path)
   (targets :initform nil)))

(defclass target ()
  ((name :initarg :name
         :accessor target-name)
   (build-file :initarg :build-file
               :accessor target-build-file)
   (deps :initarg :deps
         :accessor target-deps)))

(defclass executor ()
  ())

(defvar *config*)

(defmethod build (executor target))

(defmethod output-file (executor build-target))

(defclass config ()
  ((root :initarg :root)))

(defun config (&rest args)
  (setf *config* (apply 'make-instance
                         'config
                          args)))

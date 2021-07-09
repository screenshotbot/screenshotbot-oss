;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;;
;;;; The plan with this file is to document the entire history of the
;;;; schema. Each version will specify what changes have been made. In
;;;; the future we might use this to do sanity checks

(pkg:define-package :screenshotbot/schema/core
    (:use #:cl
          #:alexandria)
  (:export #:defschema
           #:validate-schemas))

(defvar *schemas* nil)
(defvar *validatedp* nil)

(defclass schema ()
  ((version :initarg :version)))

(defmacro defschema (&key version)
  `(progn
     (setf (assoc-value *schemas* ,version)
           (make-instance 'schema :version ,version))))

(defun validate-schemas ()
  (setf *validatedp* t))

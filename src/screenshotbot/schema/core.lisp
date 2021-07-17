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


(defun slot-schema (class)
  (loop for slot in (closer-mop:class-slots class)
        collect
        (let ((sym (closer-mop:slot-definition-name slot)))
          `(("name" . ,(string sym))
            ("package" . ,(package-name (symbol-package sym)))))))

(defun schema-of-class (class)
  `(("name" . ,(string (class-name class)))
    ("package" . ,(string (package-name (symbol-package (class-name class)))))
    ("slots" . ,(slot-schema class))))

(defun full-schema ()
  (loop for parent in '(bknr.datastore:store-object util:object-with-oid)
        appending
        (loop for class in (closer-mop:class-direct-subclasses (find-class parent))
              collect (schema-of-class class))))

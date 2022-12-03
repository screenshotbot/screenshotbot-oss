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
  (:export #:defsecret
           #:secret))
(in-package :screenshotbot/secret)

(defvar *secret-list* nil)
(defvar *secrets* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *secret-dtd* (asdf:system-relative-pathname :screenshotbot "dtd/secret.dtd")))

(defvar *secret-file* (asdf:system-relative-pathname :screenshotbot "../../.secrets/secrets.xml"))

(defclass secret ()
  ((name :initarg :name
         :element "name"
         :reader secret-name)
   (value :initarg :value
          :element "value"
          :reader secret-value)
   (public :initarg :public
           :initform nil
           :element "public-id"
           :reader secret-public-id)
   (environment :initarg :environment
                :initform "production"
                :attribute "environment"
                :reader secret-environment))
  (:dtd-name *secret-dtd*)
  (:element "secret")
  (:metaclass bknr.impex:xml-class))

(defmethod print-object ((self secret) out)
  (format out "#<SECRET ~a [~a]>" (secret-name self)
          (secret-environment self)))

(defun read-secrets ()
  (let ((file *secret-file*))
    (when (uiop:file-exists-p file)
      (second
       (parse-xml-file file
                       (list (find-class 'secret)))))))

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

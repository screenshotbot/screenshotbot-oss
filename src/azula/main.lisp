;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :azula/main
  (:use :cl)
  (:export :config
   :azula-root
           :build))
(in-package :azula/main)

(defclass build-file ()
  ((canonical-name :initarg :canonical-name
                   :accessor canonical-name)
   (targets :initform nil)))

(defclass target ()
  ((name :initarg :name
         :accessor target-name)
   (build-file :initarg :build-file
               :accessor target-build-file)
   (deps :initarg :deps
         :initform nil
         :accessor target-deps)
   (srcs :initarg :srcs
         :initform nil
         :accessor target-srcs)))

(defvar *current-build-file*)

(defclass executor ()
  ())

(defvar *targets* nil)

(defvar *config*)

(defmethod output-file (executor build-target))

(defclass config ()
  ((root :initarg :root
         :accessor config-root)))

(defun azula-root ()
  (config-root *config*))

(defun config (&rest args)
  (setf *config* (apply 'make-instance
                         'config
                          args)))

(defun break-target (target-name)
  (assert (str:starts-with-p "//" target-name))
  (let ((res (str:split ":" name)))
    (assert (equal 2 (length res)))
    res))

(defun build-file-path (build-file)
  (path:catfile (config-root *config*)
                (format nil "~a/" (str:substring 2 nil (namestring build-file)))
                "AZULA"))

(defun load-build-file (build-file canonical-name)
  (let ((*package* (find-package :azula/build-file-env))
        (*current-build-file* (make-instance 'build-file :canonical-name canonical-name)))
   (load build-file)))

(defun load-target (target)
  )

(defmacro define-target (name type)
  `(defun ,name (&rest args)
     (push (apply 'make-instance ',type
                  :build-file *current-build-file*
                  args) *targets*)))

(defmethod canonical-name ((target target))
  (let ((build-file (target-build-file target)))
    (format nil "~a:~a" (canonical-name build-file)
            (target-name target))))

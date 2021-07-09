;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/java/reader
  (:use #:cl)
  (:import-from ./java
                #:invoke
                #:%%read-java-field
                #:array->list)
  (:export #:java-list->list
           #:java-syntax))

(defun read-case-sensitive-symbol (stream char recursive)
  (declare (ignore recursive char))
  (let ((*readtable* (copy-readtable nil))
        (*package* (find-package "KEYWORD")))
    (setf (readtable-case *readtable*) :preserve)
    (read stream t t)))

(defun read-invoke (stream char recursive)
  (declare (ignore recursive))
  (let ((name (read-case-sensitive-symbol stream char t)))
    `(lambda (obj &rest args)
       (apply 'invoke obj ',name args))))

(defun read-comma-syntax (stream char recursive)
  `(quote ,(read-case-sensitive-symbol stream char recursive)))

(named-readtables:defreadtable java-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\_ 'read-invoke)
  (:dispatch-macro-char #\# #\, 'read-comma-syntax))

(defun java-list->list (java-list)
  (array->list (invoke java-list :|toArray|)))

(defun read-java-field (obj name)
  (%%read-java-field obj (string name)))

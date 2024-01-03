;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/symbol-detector
  (:use #:cl)
  (:export
   #:detect))
(in-package :util/symbol-detector)

(defun detect (filename)
  (let ((result (make-hash-table))
        (package (find-package "CL"))
        (readtable *readtable*))
    (labels ((process-symbols (expr)
               (cond
                 ((symbolp expr)
                  (unless (eql package (symbol-package expr))
                    (setf (gethash (symbol-package expr) result)
                          expr)))
                 ((consp expr)
                  (process-symbols (car expr))
                  (process-symbols (cdr expr))))))

      (with-open-file (stream filename :direction :input)
        (loop for expr = (let ((*package* package)
                               (*readtable* readtable))
                           (read stream nil nil))
              while expr
              if (member (car expr)
                         '(defpackage uiop:define-package))
                do
                   (log:info "using package ~s" (second expr))
                   (setf package (find-package (second expr)))

              if (member (car expr)
                         '(named-readtables:in-readtable
                           markup:enable-reader))
                do
                   (setf readtable (named-readtables::ensure-readtable
                                    (or (second expr)
                                        'markup:syntax)))
              else
                do
                   (process-symbols expr)))
      (loop for package being the hash-keys of result
            collect (cons package (gethash package result))))))

;; (detect "/home/arnold/builds/web/src/screenshotbot/login/common.lisp")

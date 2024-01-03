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
                          (list*
                           expr
                           (gethash (symbol-package expr) result)))))
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
      (values
       (loop for package being the hash-keys of result
             collect (cons package (remove-duplicates (gethash package result))))
       package))))

;; (detect "/home/arnold/builds/web/src/screenshotbot/login/common.lisp")

(defun generate-defpackage (file)
  (with-output-to-string (*standard-output*)
    (multiple-value-bind (package-map package)
        (detect file)
      (format t "(defpackage :~a
  (:use :cl)~%" (string-downcase (package-name package)))
      (loop for (package . symbols) in (sort (loop for (p . x) in package-map
                                                   if p
                                                     collect (cons p x))
                                             #'string<
                                             :key (lambda (x) (package-name (car x))))
            do
               (format t "(:import from #:~a" (string-downcase (package-name package)))
               (loop for symbol in (sort symbols #'string< :key #'symbol-name)
                     do (format t "~%   #:~a" (string-downcase (symbol-name symbol))))
               (format t ")~%")))
    (format t ")~%")))

;; (format t "~a" (generate-defpackage "/home/arnold/builds/web/src/screenshotbot/login/common.lisp"))

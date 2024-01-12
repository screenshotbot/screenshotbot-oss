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

(defun fix-content (content)
  (let* ((content (str:replace-all "#_" "__" content) #| java |#)
         (content (str:replace-all "#:" "__" content))
         (content (str:replace-all ":" "_" content)))
    content))

(defun detect (&key filename content)
  (let* ((result (make-hash-table))
         (package (find-package "CL"))
         (readtable *readtable*)
         (content (or
                   content
                   (uiop:read-file-string filename))))
    (log:info "Content is: ~a" content)
    (labels ((process-symbols (expr)
               (cond
                 ((symbolp expr)
                  (unless (or
                           (eql package (symbol-package expr))
                           (str:containsp "_" (symbol-name expr))
                           (member expr
                                   '(markup/markup::make-escaped
                                     markup/markup::make-merge-tag
                                     markup/markup::make-toplevel-node
                                     markup/markup::make-xml-tag
                                     uiop:define-package
                                     system::bq-list
                                     system::bq-append
                                     system::bq-list*)))
                    (setf (gethash (symbol-package expr) result)
                          (list*
                           expr
                           (gethash (symbol-package expr) result)))))
                 ((consp expr)
                  (process-symbols (car expr))
                  (process-symbols (cdr expr))))))

      (process-symbols (get-external-symbols package))
      (let ((stream (make-string-input-stream content)))
        (loop for expr = (let ((*package* package)
                               (*readtable* readtable))
                           (read stream nil nil))
              while expr
              if (member (car expr)
                         '(defpackage uiop:define-package))
                do
                   (log:info "using package ~s" (second expr))
                   (setf package (find-package (second expr)))
                   (setf stream (make-string-input-stream
                                 (fix-content
                                  (uiop:slurp-input-stream 'string stream))))
              if (or
                  (member (car expr)
                          '(named-readtables:in-readtable
                            markup:enable-reader))
                  (string-equal
                   "markup_enable-reader" (string-downcase (symbol-name (car expr))))
                  (string-equal
                   "named-readtables_in-readtable" (string-downcase (symbol-name (car expr)))))
                do
                   (setf readtable (named-readtables::ensure-readtable
                                    (let ((rt (second expr)))
                                      (cond
                                        ((string-equal "markup_syntax" (string rt))
                                         'markup:syntax)
                                        (t
                                         'markup:syntax)))))
              else
                do
                   (process-symbols expr)))
      (values
       (loop for package being the hash-keys of result
             collect (cons package (remove-duplicates (gethash package result))))
       package))))

;; (detect "/home/arnold/builds/web/src/screenshotbot/login/common.lisp")

(defun get-external-symbols (package)
  (let ((res))
    (do-external-symbols (sym package)
      (push sym res))
    (sort res #'string<)))

(defun generate-defpackage (filename)
  (with-output-to-string (*standard-output*)
    (multiple-value-bind (package-map package)
        (detect :filename filename)
      (format t "(defpackage :~a
  (:use :cl)~%" (string-downcase (package-name package)))
      (loop for (package . symbols) in (sort (loop for (p . x) in package-map
                                                   if p
                                                     collect (cons p x))
                                             #'string<
                                             :key (lambda (x) (package-name (car x))))
            unless (or
                    (eql (find-package :keyword)
                         package)
                    (eql (find-package :cl)
                         package))
            do
               (format t "(:import-from #:~a" (string-downcase (package-name package)))
               (loop for symbol in (sort symbols #'string< :key #'symbol-name)
                     do (format t "~%   #:~a" (string-downcase (symbol-name symbol))))
               (format t ")~%"))
      (cond
        ((get-external-symbols package)
         (format t "(:export ")
         (dolist (sym (get-external-symbols package))
           (format t "~%   #:~a" (string-downcase (symbol-name sym))))
         (format t ")"))
        (t
         (file-position *standard-output* (1- (file-position *standard-output*)))))
      #+lispworks
      (alexandria:when-let ((nicknames (hcl:package-local-nicknames package)))
        (setf nicknames (sort (copy-seq nicknames) #'string< :key #'car))
        (format t "~%")
        (format t "  (:local-nicknames ")
        (format t "~a"
                (str:join #\Newline
                          (loop for (key . package) in nicknames
                                collect (format nil "(#:~a #:~a)" (string-downcase key)
                                                (string-downcase (package-name package))))))
        (format t ")")))

    (format t ")~%")))

;; (format t "~a" (generate-defpackage "/home/arnold/builds/web/src/screenshotbot/login/forgot-password.lisp"))

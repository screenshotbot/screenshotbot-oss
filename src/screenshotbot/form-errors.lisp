;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop/package:define-package :screenshotbot/form-errors
    (:use #:cl #:alexandria #:mquery)
  (:export #:with-form-errors))
(in-package :screenshotbot/form-errors)

(markup:enable-reader)

(defun %with-form-errors (html &key errors args was-validated)
  (mquery:with-document (html)
    (when was-validated
      (mquery:add-class ($ "input") "is-valid")
      (dolist (arg args)
        (destructuring-bind (name . val) arg
          (setf (mquery:val ($ (mquery:namequery name)))
                val)))
      (dolist (err errors)
        (when (consp err)
         (destructuring-bind (name . msg) err
           (let ((input ($ (mquery:namequery name))))
             (assert input)
             (mquery:remove-class input "is-valid")
             (mquery:add-class input "is-invalid")
             (setf (mquery:after
                    (cond
                      ((equal "checkbox" (mquery:attr input "type"))
                       ;; for checkbox there's a label after the input form
                       (mquery:after input))
                      (t
                       input)))
                   <div class= "invalid-feedback">,(progn msg)</div>)))))))
  html)

(defmacro with-form-errors ((&rest args &key errors was-validated &allow-other-keys) &body body)
  (let* ((args (plist-alist args)))
    `(%with-form-errors (progn ,@body)
                        :errors ,errors
                        :was-validated ,was-validated
                        :args (list ,@(loop for x in args collect
                                          `(cons ,(car x) ,(cdr x)))))))

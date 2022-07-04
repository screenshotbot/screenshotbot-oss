;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop/package:define-package :util/form-errors
    (:use #:cl #:alexandria #:mquery)
  (:export #:with-form-errors
           #:update-form-values))
(in-package :util/form-errors)

(markup:enable-reader)

(defun update-form-values (args)
  (dolist (arg args)
    (destructuring-bind (name . val) arg
      (log:info "Setting ~S" arg)
      (setf (mquery:val ($ (mquery:namequery name)))
            val))))

(defun %with-form-errors (html &key errors args args-list was-validated tooltip)
  (let ((args (append args args-list)))
   (mquery:with-document (html)
     (when was-validated
       (mquery:add-class ($ "input") "is-valid")
       (update-form-values
        args)
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
                     <div class= (if tooltip "invalid-tooltip" "invalid-feedback") >,(progn msg)</div>))))))))
  (values html errors))

(defmacro with-form-errors ((&rest args &key errors args-list was-validated tooltip &allow-other-keys) &body body)
  (let* ((args (plist-alist args)))
    `(%with-form-errors (progn ,@body)
                        :errors ,errors
                        :was-validated ,was-validated
                        :tooltip ,tooltip
                        :args (list ,@(loop for x in args collect
                                            `(cons ,(car x) ,(cdr x))))
                        :args-list ,args-list)))

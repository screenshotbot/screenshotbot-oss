;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop/package:define-package :util/form-errors
    (:use #:cl #:alexandria #:mquery)
  (:import-from #:markup
                #:xml-tag-children)
  (:export #:with-form-errors
           #:update-form-values
           #:with-error-builder))
(in-package :util/form-errors)

(markup:enable-reader)

(defun update-form-values (args)
  (dolist (arg args)
    (destructuring-bind (name . val) arg
      (log:info "Setting ~S" arg)
      (setf (mquery:val ($ (mquery:namequery name)))
            val))))

(defun input-is-button-p (input)
  (str:s-member '("button" "submit")
                (mquery:attr input "type")))

(defun %with-form-errors (html &key errors args args-list was-validated tooltip)
  (let ((args (append args args-list)))
   (mquery:with-document (html)
     (when was-validated
       (mquery:add-class (remove-if #'input-is-button-p ($ "input")) "is-valid")
       (update-form-values
        args)
       (dolist (err errors)
         (cond
           ((consp err)
            (destructuring-bind (name . msg) err
              (let ((input ($ (mquery:namequery name))))
                (assert input)
                (mquery:remove-class input "is-valid")
                (mquery:add-class input "is-invalid")
                (let ((input-group (let ((parent (mquery:parent input)))
                                     (when (mquery:has-class-p parent "input-group")
                                       parent))))
                  (when input-group
                    (mquery:add-class input-group "has-validation"))
                  (setf (mquery:after
                         (cond
                           ((equal "checkbox" (mquery:attr input "type"))
                            ;; for checkbox there's a label after the input form
                            (mquery:after input))
                           (input-group
                            (car (last (xml-tag-children (mquery:parent input)))))
                           (t
                            input)))
                        <div class= (if tooltip "invalid-tooltip" "invalid-feedback") >,(progn msg)</div>)))))
           (t
            ;; Add error message to a alert
            (let ((alert ($ ".alert")))
              (mquery:remove-class alert "d-none")
              (mquery:mqappend alert
                               <div>,(progn err)</div>))
            ))))))
  (values html errors))

(defmacro with-form-errors ((&rest args &key errors args-list was-validated tooltip &allow-other-keys) &body body)
  "Update the body to add bootstrap based error validation to every field. Note that global errors, need a div with class `alert alert-danger`."
  (let* ((args (plist-alist args)))
    `(%with-form-errors (progn ,@body)
                        :errors ,errors
                        :was-validated ,was-validated
                        :tooltip ,tooltip
                        :args (list ,@(loop for x in args collect
                                            `(cons ,(car x) ,(cdr x))))
                        :args-list ,args-list)))

(defmacro with-error-builder ((&key (check (error "must provide :check function name"))
                                 (errors (error "must provide a variable for the errors"))
                                 form-builder
                                 form-args
                                 success)
                              &body body)
  `(let ((,errors nil))
     (flet ((,check (field expr value)
              (unless expr
                (push (cons field value)
                      ,errors))))
       ,@body
       (cond
         (,errors
          (with-form-errors (:errors ,errors
                             ,@form-args
                             :was-validated t)
            ,form-builder))
         (t
          ,success)))))

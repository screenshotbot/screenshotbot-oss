;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/fake-fli
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:define-c-typedef
   #:register-module
   #:define-c-struct
   #:define-foreign-function
   #:define-c-enum
   #:convert-from-foreign-string
   #:null-pointer-p
   #:with-dynamic-foreign-objects
   #:foreign-slot-value
   #:incf-pointer))
(in-package :util/fake-fli)

(defun fix-type (x)
  (cond
    ((eql :size-t x)
     :uint64)
    ((not (listp x))
     x)
    ((equal '(:reference-pass :ef-mb-string) x)
     :string)
    ((equal '(:reference :ef-mb-string) x)
     :string)
    (t
     x)))

(defmacro define-c-typedef (name type)
  `(cffi:defctype ,name ,(fix-type type)))

(defun register-module (name &key real-name)
  (declare (ignore name))
  (cffi:load-foreign-library real-name))

(defmacro define-c-struct (name &rest fields)
  `(cffi:defcstruct ,name
     ,@ (loop for (name type) in fields
              collect (list name (fix-type type)))))

(defmacro define-foreign-function (name args &key result-type)
  (destructuring-bind (name-var name) (cond
                                        ((symbolp name)
                                         (list name (str:replace-all
                                                     "-" "_"
                                                     (str:downcase (string name)))))
                                        (t
                                         name))
   (multiple-value-bind (args reference-returns) (parse-reference-returns args)
     (let ((cffi-name (intern (format nil "%~a-native" name))))
       `(progn
          (cffi:defcfun (,name ,cffi-name) ,(fix-type result-type)
            ,@(loop for arg in args
                    collect
                    (destructuring-bind (name type) arg
                      (list name (fix-type type)))))
          (defun ,name-var (,@ (mapcar #'car args))
            ,(wrap-reference-returns
              reference-returns
              (let ((returns (loop for x in reference-returns
                                   collect
                                   `(cffi:mem-ref ,(car x) ',(cadr (cadr x)))
                                   )))
                `(let ((ret (,cffi-name ,@ (mapcar #'car args))))
                   (values
                    ret
                    ,@ returns))))))))))

(defun wrap-reference-returns (reference-returns content)
  (cond
    ((null reference-returns)
     content)
    (t
     (destructuring-bind (next &rest rest) reference-returns
       (let ((type (cadr (cadr next))))
        `(let ((original-value ,(car next)))
           (cffi:with-foreign-object (,(car next)
                                      ',type)
             (setf (cffi:mem-ref ,(car next) ',type)
                   original-value)
             ,(wrap-reference-returns rest content))))))))


(defun parse-reference-returns (args)
  (let (final-args
        final-rets)
    (dolist (arg args)
      (destructuring-bind (name type) arg
          (cond
            ((and (listp type) (eql :reference-return (first type)))
             (push (list name :pointer) final-args)
             (push arg final-rets))
            (t
             (push arg final-args)))))
    (values (reverse final-args)
            (reverse final-rets))))

(defmacro define-c-enum (&rest rest)
  `(cffi:defcenum ,@rest))

(defun convert-from-foreign-string (x)
  (cffi:convert-from-foreign x :string))

(defun null-pointer-p (x)
  (cffi:null-pointer-p x))


(defmacro with-dynamic-foreign-objects (((output type &key nelems)) &body body)
  `(cffi:with-foreign-object (,output ',type ,nelems)
     ,@body))

(defmacro foreign-slot-value (obj type slot)
  `(cffi:foreign-slot-value ,obj ,type ,slot))

(defun incf-pointer (pointer)
  `(cffi:inc-pointer ,pointer))

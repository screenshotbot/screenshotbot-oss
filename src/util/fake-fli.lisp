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
   #:incf-pointer
   #:malloc
   #:free
   #:dereference
   #:with-coerced-pointer
   #:*null-pointer*
   #:make-pointer
   #:copy-pointer))
(in-package :util/fake-fli)

(defvar *type-map* (trivial-garbage:make-weak-hash-table :weakness :key
                                                         #+sbcl
                                                                   :synchronized
                                                         #+sbcl
                                                         t))

(defun make-typed-pointer (&key ptr type)
  (let ((ptr (cffi:make-pointer (cffi:pointer-address ptr))))
    (setf (gethash ptr *type-map*) type)
    ptr))



(defun typed-pointer-type (ptr)
  (or
   (gethash ptr *type-map*)
   :void))

(defun typed-pointer-ptr (ptr)
  ptr)

(defun fix-type (x &key (allow-count t))
  "Returns two values the converted type, and the count, in case of an array"
  #+nil(log:info "fixing type: ~S" x)
  (cond
    ((not x)
     (error "this looks like a bad type: ~a" x))
    ((eql :size-t x)
     :uint64)
    ((eql :ssize-t x)
     :int64)
    ((eql :time-t x)
     :uint64)
    ((not (listp x))
     x)
    ((equal '(:reference-pass :ef-mb-string) x)
     :string)
    ((equal '(:reference :ef-mb-string) x)
     :string)
    ((equal :c-array (car x))
     (unless allow-count
       (Error "count not supported in this location"))
     (assert (fix-type (cadr x)))
     (values (fix-type (cadr x)) (caddr x)))
    ((equal :pointer (car x))
     :pointer)
    (t
     x)))

(defmacro define-c-typedef (name type)
  (multiple-value-bind (type count) (fix-type type)
    (assert (not count))
   `(cffi:defctype ,name ,type)))

(defun register-module (name &key real-name file-name)
  (declare (ignore name))
  (cffi:load-foreign-library (or file-name real-name)))

(defmacro define-c-struct (name &rest fields)
  `(cffi:defcstruct ,name
     ,@ (loop for (name type) in fields
              collect (multiple-value-bind (type count) (fix-type type)
                        (list* name type
                               (when count
                                 (list :count count)))))))

(defmacro define-foreign-function (name args &key result-type documentation)
  (assert result-type)
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
          (cffi:defcfun (,name ,cffi-name) ,(fix-type result-type :allow-count nil)
            ,@(loop for arg in args
                    collect
                    (destructuring-bind (name type) arg
                      (list name (fix-type type :allow-count nil)))))
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

(defun make-pointer (&key address type)
  (make-typed-pointer
   :ptr
   (cffi:make-pointer address)
   :type type))

(defmacro %with-dynamic-foreign-objects (((output type &key (nelems 1) (fill 0))) &body body)
  ;; fill is ignored!
  `(cffi:with-foreign-object (,output ',type ,nelems)
     (let ((,output (make-typed-pointer :ptr ,output :type ',type)))
      ,@body)))

(defmacro with-dynamic-foreign-objects (exprs &body body)
  (cond
    (exprs
     `(%with-dynamic-foreign-objects (,(car exprs))
        (with-dynamic-foreign-objects ,(cdr exprs)
          ,@body)))
    (t
     `(progn ,@body))))

(defmacro foreign-slot-value (obj slot)
  `(cffi:foreign-slot-value (typed-pointer-ptr ,obj)
                            (typed-pointer-type ,obj)
                            ,slot))

(defmacro incf-pointer (pointer)
  `(setf
    ,pointer
    (let ((pointer ,pointer))
     (make-typed-pointer
      :ptr (cffi:inc-pointer (typed-pointer-ptr pointer)
                                 (cffi:foreign-type-size (typed-pointer-type pointer)))
      :type (typed-pointer-type pointer)))))

(defun copy-pointer (pointer)
  (make-typed-pointer
   :ptr (typed-pointer-ptr pointer)
   :type (typed-pointer-type pointer)))

(defun dereference (obj)
  (cffi:mem-ref (typed-pointer-ptr obj) (typed-pointer-type obj)))

(defun malloc (&key type)
  (make-typed-pointer
   :ptr (cffi:foreign-alloc (fix-type type :allow-count nil))
   :type type))

(Defun free (obj)
  (cffi:foreign-free
   (typed-pointer-ptr obj)))

(defmacro with-coerced-pointer ((output &key type) input &body body)
  `(let ((,output (make-typed-pointer
                   :ptr (typed-pointer-ptr ,output)
                   :type ,type)))
     ,@body))

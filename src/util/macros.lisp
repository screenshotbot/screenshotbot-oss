;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/macros
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:defblock))
(in-package :util/macros)

(define-condition unsupported-lambda-list (error)
  ((reason :initarg :reason)))

(defun build-funcall (fn-name real-fn-args fn-arg-values body)
  `(,fn-name (lambda (,@ (get-bindings real-fn-args fn-arg-values)) ,@body)
             ,@ (get-non-bindings real-fn-args fn-arg-values)))

(defun get-bindings (real-fn-args fn-arg-values)
  (let ((fn-args (remove-&fn real-fn-args)))
    (eval
     `(destructuring-bind
          ,(loop for x in fn-args
                 if (binding-sym-p x)
                   collect (name x)
                 else
                   collect x)
          ',fn-arg-values
        (list ,@ (loop for x in fn-args
                       if (binding-sym-p x)
                         collect (name x)))))))

(defun get-non-bindings (real-fn-args fn-arg-values)
  "Carefully remove all the fn-arg-values that correspond to bindings"
  (let ((fn-args (remove-&fn real-fn-args)))
    (labels ((%get-non-bindings (args value-exprs keysp)
               (log:info "Looking at ~a ~a" args value-exprs)
               (cond
                 ((null args)
                  value-exprs)
                 ((binding-sym-p (car args))
                  (%get-non-bindings (cdr args)
                                     (cdr value-exprs)
                                     keysp))
                 ((eql (car args) 'cl:&key)
                  ;; Currently not supporting &binding in the keys section
                  value-exprs)
                 (t
                  (list*
                   (car value-exprs)
                   (%get-non-bindings (cdr args)
                                      (cdr value-exprs)
                                      keysp))))))
      (%get-non-bindings fn-args fn-arg-values nil))))

(defun is-sym (sym looking-for)
  (when (and
         (symbolp sym)
         (symbolp looking-for))
    (string= (string sym) (string looking-for))))

(defclass binding-sym ()
  ((name :initarg :name
         :reader name)))

(defun binding-sym-p (x)
  (typep x 'binding-sym))

(defun check-validity (args)
  (labels ((%check (args seen-key seen-rest)
             (when args
              (destructuring-bind (next &rest rest) args
                (cond
                  ((is-sym next '&binding)
                   (when seen-key
                     (error 'unsupported-lambda-list
                             :reason "&binding not supported after &key"))
                   (when seen-rest
                     (error 'unsupported-lambda-list
                             :reason "&binding not supported after &rest"))
                   (%check (cdr rest) seen-key seen-rest))
                  ((eql next 'cl:&key)
                   (%check rest t seen-rest))
                  ((eql rest 'cl:&rest)
                   (%check rest seen-key t))
                  (t
                   (%check rest seen-key seen-rest))))))
           )
    (%check args nil nil)))

(defun remove-&fn (args)
  (check-validity args)
  (let ((fn nil))
    (let ((result
           (loop while args
                 for next = (car args)
                 if (is-sym next '&fn)
                   do
                      (setf fn (cadr args))
                      (setf args (cddr args))
                 else if (is-sym next '&binding)
                      collect
                      (prog1
                          (make-instance 'binding-sym :name (cadr args))
                        (setf args (cddr args)))
                 else
                   collect
                   (progn
                     (setf args (cdr args))
                     next))))
      (values result fn))))

(defun remove-binding-syms (args)
  (loop for x in args
        if (not (typep x 'binding-sym))
          collect x into final-arg
        else
          collect x into binding-syms
        finally (return (values final-arg binding-syms))))


(defmacro defblock (name real-fn-args  &body body)
  (let ((fn-name (intern (format nil "CALL-~a" (string name)) *package*)))
    (multiple-value-bind (fn-args body-fn) (remove-&fn real-fn-args)
     `(progn
        (defun ,fn-name (,body-fn ,@ (remove-binding-syms fn-args))
          ,@body)
        (defmacro ,name ((&rest fn-arg-values) &body macro-body)
          (build-funcall ',fn-name ',real-fn-args fn-arg-values macro-body))))))

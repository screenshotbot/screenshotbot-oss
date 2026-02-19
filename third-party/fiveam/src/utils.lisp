;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: FIVEAM; Base: 10; -*-

(in-package :it.bese.fiveam)

(defmacro dolist* ((iterator list &optional return-value) &body body)
  "Like DOLIST but destructuring-binds the elements of LIST.

If ITERATOR is a symbol then dolist* is just like dolist EXCEPT
that it creates a fresh binding."
  (if (listp iterator)
      (let ((i (gensym "DOLIST*-I-")))
        `(dolist (,i ,list ,return-value)
           (destructuring-bind ,iterator ,i
             ,@body)))
      `(dolist (,iterator ,list ,return-value)
         (let ((,iterator ,iterator))
           ,@body))))

(defun make-collector (&optional initial-value)
  "Create a collector function.

A Collector function will collect, into a list, all the values
passed to it in the order in which they were passed. If the
callector function is called without arguments it returns the
current list of values."
  (let ((value initial-value)
        (cdr (last initial-value)))
    (lambda (&rest items)
      (if items
          (progn
            (if value
                (if cdr
                    (setf (cdr cdr) items
                          cdr (last items))
                    (setf cdr (last items)))
                (setf value items
                      cdr (last items)))
            items)
          value))))

(defun partitionx (list &rest lambdas)
  (let ((collectors (mapcar (lambda (l)
                              (cons (if (and (symbolp l)
                                             (member l (list :otherwise t)
                                                     :test #'string=))
                                        (constantly t)
                                        l)
                                    (make-collector)))
                            lambdas)))
    (dolist (item list)
      (block item
        (dolist* ((test-func . collector-func) collectors)
          (when (funcall test-func item)
            (funcall collector-func item)
            (return-from item)))))
    (mapcar #'funcall (mapcar #'cdr collectors))))

;;;; ** Anaphoric conditionals

(defmacro if-bind (var test &body then/else)
  "Anaphoric IF control structure.

VAR (a symbol) will be bound to the primary value of TEST. If
TEST returns a true value then THEN will be executed, otherwise
ELSE will be executed."
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro aif (test then &optional else)
  "Just like IF-BIND but the var is always IT."
  `(if-bind it ,test ,then ,else))

;;;; ** Simple list matching based on code from Paul Graham's On Lisp.

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (with-gensyms (val foundp)
        (destructuring-bind ((test &rest progn) &rest others)
            clauses
          `(multiple-value-bind (,val ,foundp)
               ,test
             (if (or ,val ,foundp)
                 (let ((it ,val))
                   (declare (ignorable it))
                   ,@progn)
                 (acond2 ,@others)))))))

(defun varsymp (x)
  (and (symbolp x)
       (let ((name (symbol-name x)))
         (and (>= (length name) 2)
              (char= (char name 0) #\?)))))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defun list-match (x y &optional binds)
  (acond2
    ((or (eql x y) (eql x '_) (eql y '_))
     (values binds t))
    ((binding x binds) (list-match it y binds))
    ((binding y binds) (list-match x it binds))
    ((varsymp x) (values (cons (cons x y) binds) t))
    ((varsymp y) (values (cons (cons y x) binds) t))
    ((and (consp x) (consp y) (list-match (car x) (car y) binds))
     (list-match (cdr x) (cdr y) it))
    (t (values nil nil))))

(defun vars (match-spec)
  (let ((vars nil))
    (labels ((find-vars (spec)
               (cond
                 ((null spec) nil)
                 ((varsymp spec) (push spec vars))
                 ((consp spec)
                  (find-vars (car spec))
                  (find-vars (cdr spec))))))
      (find-vars match-spec))
    (delete-duplicates vars)))

(defmacro list-match-case (target &body clauses)
  (if clauses
      (destructuring-bind ((test &rest progn) &rest others)
          clauses
        (with-gensyms (tgt binds success)
          `(let ((,tgt ,target))
             (multiple-value-bind (,binds ,success)
                 (list-match ,tgt ',test)
               (declare (ignorable ,binds))
               (if ,success
                   (let ,(mapcar (lambda (var)
                                   `(,var (cdr (assoc ',var ,binds))))
                                 (vars test))
                     (declare (ignorable ,@(vars test)))
                     ,@progn)
                   (list-match-case ,tgt ,@others))))))
      nil))

;;;; * def-special-environment

(defun check-required (name vars required)
  (dolist (var required)
    (assert (member var vars)
            (var)
            "Unrecognized symbol ~S in ~S." var name)))

(defmacro def-special-environment (name (&key accessor binder binder*)
                                  &rest vars)
  "Define two macros for dealing with groups or related special variables.

ACCESSOR is defined as a macro: (defmacro ACCESSOR (VARS &rest
BODY)).  Each element of VARS will be bound to the
current (dynamic) value of the special variable.

BINDER is defined as a macro for introducing (and binding new)
special variables. It is basically a readable LET form with the
prorpe declarations appended to the body. The first argument to
BINDER must be a form suitable as the first argument to LET.

ACCESSOR defaults to a new symbol in the same package as NAME
which is the concatenation of \"WITH-\" NAME. BINDER is built as
\"BIND-\" and BINDER* is BINDER \"*\"."
  (unless accessor
    (setf accessor (format-symbol (symbol-package name) "~A-~A" '#:with name)))
  (unless binder
    (setf binder   (format-symbol (symbol-package name) "~A-~A" '#:bind name)))
  (unless binder*
    (setf binder*  (format-symbol (symbol-package binder) "~A~A" binder '#:*)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (flet ()
       (defmacro ,binder (requested-vars &body body)
         (check-required ',name ',vars (mapcar #'car requested-vars))
         `(let ,requested-vars
            (declare (special ,@(mapcar #'car requested-vars)))
            ,@body))
       (defmacro ,binder* (requested-vars &body body)
         (check-required ',name ',vars (mapcar #'car requested-vars))
         `(let* ,requested-vars
            (declare (special ,@(mapcar #'car requested-vars)))
            ,@body))
       (defmacro ,accessor (requested-vars &body body)
         (check-required ',name ',vars requested-vars)
         `(locally (declare (special ,@requested-vars))
            ,@body))
       ',name)))

;; Copyright (c) 2002-2006, Edward Marco Baringer
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE

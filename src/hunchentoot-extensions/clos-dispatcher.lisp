;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/clos-dispatcher
  (:use #:cl)
  (:import-from #:hunchentoot-extensions
                #:def-clos-dispatch
                #:clos-dispatcher)
  (:export
   #:clos-dispatcher))
(in-package :hunchentoot-extensions/clos-dispatcher)

(defun %make-hash-table (&rest args)
  (apply #'make-hash-table
         #+sbcl #+sbcl
         :synchronized t
         args))

(defvar *bindings* (%make-hash-table :test #'equal))

(defvar *lock* (bt:make-lock))

(defclass clos-dispatcher ()
  ()
  (:documentation "An acceptor mixin that let's you create routes for scripts using CLOS."))


(defmacro def-clos-dispatch (((var class) script-name) vars &body body)
  (when vars
    (error "Using vars is not supported yet"))
  `(bt:with-lock-held (*lock*)
     (let ((old
             (util/misc:or-setf
              (gethash ,script-name *bindings*)
              (%make-hash-table :test #'eql))))
       (setf
        (gethash (find-class ',class) old)
        (lambda (,var)
          (declare (ignorable ,var))
          ,@body)))))

(defgeneric %normalize (input)
  (:method ((input string))
    input)
  (:method ((input markup:abstract-xml-tag))
    (markup:write-html input)))

(defmethod dispatch-clos-request ((self clos-dispatcher) dispatcher)
  (setf (hunchentoot:header-out "X-clos-acceptor") "1")
  (%normalize (funcall dispatcher self)))

(defmethod hunchentoot:acceptor-dispatch-request ((self clos-dispatcher) request)
  (let ((script-name (hunchentoot:script-name request)))
    (let ((binding (gethash script-name *bindings*)))
      (cond
        (binding
         (let ((classes (closer-mop:class-precedence-list (class-of self))))
           (loop for class in classes
                 for dispatcher = (gethash class binding)
                 if dispatcher
                   return (dispatch-clos-request self dispatcher)
                 finally
                 (call-next-method))))
        (t
         (call-next-method))))))

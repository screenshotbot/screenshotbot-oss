;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/auto-restart
  (:use #:cl
        #:iterate)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:with-auto-restart))
(in-package :util/auto-restart)


(define-condition restart-already-defined (error)
  ((restart-name :initarg :restart-name))
  (:documentation "When calling with-auto-restart, we expect the
  restart to be defined inside the body, not before it."))

(defmacro with-auto-restart ((&key (retries nil)
                                (restart-name))
                             &body body)
  (assert (= 1 (length body)))

  (let ((body (car body)))
    (let* ((args-pos (position-if #'listp body))
           (before-args (subseq body 0 (1+ args-pos)))
           (fn-name (cadr before-args))
           (fn-args (elt body args-pos ))
           (var-names (loop for var in fn-args
                            if (listp var)
                              collect (car var)
                            else
                              collect var))
           (decls-onwards (subseq body (1+ args-pos)))
           (restart-name (or
                          restart-name
                          (intern (format nil "RETRY-~a" fn-name) *package*))))

      ;; quick validation
      (loop for arg in var-names
            do
               (assert (symbolp arg)))

      `(,@before-args
        (restart-case
            (progn
              ,@decls-onwards)
          (,restart-name ()
            (,fn-name ,@ (fix-args-for-funcall var-names))))))))

(defun fix-args-for-funcall (var-names)
  (let ((state :default))
    (iter (for var in var-names)
      (cond
        ((eql '&optional var)
         (setf state :optional))
        ((eql '&key var)
         (setf state :key))
        ((str:starts-with-p "&" (string var))
         (error "Unsupported lambda-list specifier: ~a" var))
        ((not (symbolp var))
         (error "Unsupported variable name: ~a, probably because of a
         keyword arg? [TODO(arnold): this isn't hard to implement if you do
         need this]" var))
        (t
         (case state
           (:key
            (appending `(,(intern (string var) "KEYWORD")
                         ,var)))
           (otherwise
            (collect var))))))))

(defun call-with-auto-restart (restart-name fn)
  (when (find-restart restart-name)
    (error 'restart-already-defined :restart-name restart-name))
  (funcall fn :attempt 0))

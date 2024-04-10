;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auto-restart
  (:use #:cl
        #:iterate)
  (:export
   #:with-auto-restart
   #:*global-enable-auto-retries-p*
   #:exponential-backoff))
(in-package :auto-restart)


(define-condition restart-already-defined (error)
  ((restart-name :initarg :restart-name))
  (:documentation "When calling with-auto-restart, we expect the
  restart to be defined inside the body, not before it."))

(defvar *frame* nil)

(defvar *global-enable-auto-retries-p* t
  "Globally enable or disable automatic retries. Useful for unit tests.")

(defun exponential-backoff (base)
  `(lambda (attempt)
     (expt ,base attempt)))

(defstruct frame
  attempt
  error-handler
  restart-handler)

(defun %is-error-of-type (e error-classes)
  (loop for class in error-classes
        if (typep e class)
          return t
        finally
           (return nil)))

(defmacro with-auto-restart ((&key (retries 0)
                                (sleep (exponential-backoff 2))
                                (attempt (gensym "attempt"))
                                (restart-name)
                                (auto-restartable-errors ''(error)))
                             &body body)
  "Enable auto-restarts for a DEFUN.

ATTEMPT will be a variable name that will be bound to the current
attempt. Attempts will be 1-indexed (1, 2, 3 ... ).

AUTO-RESTARTABLE-ERRORS is evaluated to get a list of error classes
for which we're allowed to auto-restart. It defaults to `'(ERROR)`.
"
  (assert (= 1 (length body)))

  (let* ((body (car body))
         (sleep (cond
                  ((numberp sleep)
                   `(lambda (attempt)
                      (declare (ignore attempt))
                      ,sleep))
                  (t
                   sleep)))
         (sleep-var (gensym "sleep"))
         (retries-var (gensym "retries"))
         (args-pos (position-if #'listp body))
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
    (multiple-value-bind (body decls doc)
        (uiop:parse-body decls-onwards :documentation t)
      `(let ((,sleep-var ,sleep)
             (,retries-var ,retries))
        (,@before-args
         ,doc
         ,@decls
         (flet ((top-level-body ()
                  (flet ((body (,attempt)
                           (declare (ignorable ,attempt))
                           ,@body))
                    (let ((attempt (frame-attempt *frame*)))
                      (setf (frame-error-handler *frame*)
                            (lambda (e)
                              (cond
                                ((and *global-enable-auto-retries-p* (< attempt ,retries)
                                      (%is-error-of-type e ',(eval auto-restartable-errors)))
                                 (let ((sleep-time (funcall ,sleep-var attempt)))
                                   (unless (= 0 sleep-time)
                                     (sleep sleep-time)))
                                 (invoke-restart ',restart-name)))))
                      (setf (frame-restart-handler *frame*)
                            (lambda ()
                              (apply #',fn-name ,@ (fix-args-for-funcall var-names))))
                      ;; If we make a call to another auto-restart
                      ;; it should not see the same frame
                      (let ((*frame* nil))
                        (body attempt))))))
           (cond
             ((null *frame*)
              (let* ((frame (make-frame
                             :attempt 1
                             :error-handler nil
                             :restart-handler (lambda ()
                                                ;; The first time we call we just call the function
                                                (top-level-body))))
                     (*frame* frame))
                (block success
                  (loop
                    (restart-case
                        (handler-bind ((error (lambda (e)
                                                (funcall (frame-error-handler frame) e))))
                          (return-from success (funcall (frame-restart-handler frame))))
                      (,restart-name ()
                        ;; Retry in our loop
                        (values)))))))
             (t
              (incf (frame-attempt *frame*))
              (top-level-body)))))))))

(defun fix-args-for-funcall (var-names)
  (let ((state :default))
    (iter (for var in (append var-names
                              ;; fake ending point
                              '(&rest nil) ))
      (cond
        ((eql :end state)
         #| do nothing |#)
        ((eql '&optional var)
         (setf state :optional))
        ((eql '&key var)
         (setf state :key))
        ((eql '&rest var)
         (setf state :rest))
        ((eql #\& (elt (string var) 0))
         (error "Unsupported lambda-list specifier: ~a" var))
        ((not (symbolp var))
         ;; TODO(arnold): this isn't hard to handle if you do need it.
         (error "Unsupported variable name: ~a, probably because of a
         keyword arg?" var))
        (t
         (case state
           (:key
            (appending `(,(intern (string var) "KEYWORD")
                         ,var)))
           (:rest
            ;; this one is interesting, we can stop looking at anything
            ;; that comes after this.
            (collect var)
            (setf state :end))
           (otherwise
            (collect var))))))))

(defun call-with-auto-restart (restart-name fn)
  (when (find-restart restart-name)
    (error 'restart-already-defined :restart-name restart-name))
  (funcall fn :attempt 0))

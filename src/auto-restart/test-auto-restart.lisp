;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :test-auto-restart
  (:use #:cl
        #:fiveam)
  (:import-from #:auto-restart
                #:*global-enable-auto-retries-p*
                #:restart-already-defined
                #:with-auto-restart))
(in-package :test-auto-restart)


(fiveam:def-suite* :test-auto-restart)

(test simple-function
  (with-auto-restart ()
    (defun foo ()
      2))
  (is (equal 2 (foo))))

(test function-has-restart-defined
  (with-auto-restart ()
    (defun foo ()
      (find-restart 'retry-foo)))

  (is-true (foo)))

(test calling-the-restart-works
  (let ((res nil))
   (with-auto-restart ()
     (defun foo ()
       (cond
         ((not (>= (length res) 3))
          (push t res)
          (invoke-restart 'retry-foo))
         (t
          res)))))
  (is (equal '(t t t)
              (foo))))


(test function-with-args
  (let ((res nil))
   (with-auto-restart ()
     (defun foo (a b c)
       (cond
         ((not (>= (length res) 3))
          (push (list a b c) res)
          (invoke-restart 'retry-foo))
         (t
          res)))))
  (is (equal '((1 2 3) (1 2 3) (1 2 3))
              (foo 1 2 3))))


(test method-with-args
  (let ((res nil))
   (with-auto-restart ()
     (defmethod dummy-method ((a symbol) b)
       (cond
         ((not (>= (length res) 3))
          (push (list a b) res)
          (invoke-restart 'retry-dummy-method))
         (t
          res)))))
  (is (equal '((:foo 1) (:foo 1) (:foo 1))
              (dummy-method :foo 1))))

(test optional-args
  (let ((res nil))
   (with-auto-restart ()
     (defun foo (a &optional (b :bar))
       (cond
         ((not (>= (length res) 3))
          (push (list a b) res)
          (invoke-restart 'retry-foo))
         (t
          res)))))
  (is (equal '((:foo :bar) (:foo :bar) (:foo :bar))
              (foo :foo))))

(test keyword-arg
  (let ((res nil))
   (with-auto-restart ()
     (defun foo (a &key (b :bar))
       (cond
         ((not (>= (length res) 3))
          (push (list a b) res)
          (invoke-restart 'retry-foo))
         (t
          res)))))
  (is (equal '((:foo :bar) (:foo :bar) (:foo :bar))
              (foo :foo))))

(test keyword-arg-with-value
  (let ((res nil))
   (with-auto-restart ()
     (defun foo (a &key (b :bar))
       (cond
         ((not (>= (length res) 3))
          (push (list a b) res)
          (invoke-restart 'retry-foo))
         (t
          res)))))
  (is (equal '((:foo 3) (:foo 3) (:foo 3))
              (foo :foo :b 3))))


(test rest-args
  (let ((res nil))
   (with-auto-restart ()
     (defun foo (a &rest b)
       (cond
         ((not (>= (length res) 3))
          (push (list a b) res)
          (invoke-restart 'retry-foo))
         (t
          res)))))
  (is (equal '((:foo (1 2)) (:foo (1 2)) (:foo (1 2)))
              (foo :foo 1 2))))

(test rest-args-and-keywords
  (let ((res nil))
   (with-auto-restart ()
     (defun foo (a &rest b &key bar)
       (cond
         ((not (>= (length res) 3))
          (push (list a b bar) res)
          (invoke-restart 'retry-foo))
         (t
          res)))))
  (is (equal '((:foo (:bar 2) 2) (:foo (:bar 2) 2) (:foo (:bar 2) 2))
              (foo :foo :bar 2))))

(test rest-args-and-keywords-with-default-keyword
  (let ((res nil))
   (with-auto-restart ()
     (defun foo (a &rest b &key (bar 10))
       (cond
         ((not (>= (length res) 3))
          (push (list a b bar) res)
          (invoke-restart 'retry-foo))
         (t
          res)))))
  (is (equal '((:foo nil 10) (:foo nil 10) (:foo nil 10))
              (foo :foo))))

(define-condition test-error (error)
  ())

(test actually-do-automatic-retries
  (let ((res nil))
    (with-auto-restart (:retries 3 :sleep 0.1)
      (defun foo ()
        "dfdf"
        (declare (inline))
        (cond
          ((not (>= (length res) 10))
           (push t res)
           (error 'test-error))
          (t
           res))))

    (signals test-error
      (foo))
    (is (equal '(t t t)
                res))))

(test but-we-dont-retry-if-the-global-flag-is-off
  (let ((res nil)
        (*global-enable-auto-retries-p* nil))
    (with-auto-restart (:retries 3)
      (defun foo ()
        "dfdf"
        (declare (inline))
        (cond
          ((not (>= (length res) 10))
           (push t res)
           (error 'test-error))
          (t
           res))))

    (signals test-error
      (foo))
    (is (equal '(t)
                res))))

(test actually-do-automatic-retries-with-fake-sleep-happy-path
  (let ((res nil))
    (with-auto-restart (:retries 3 :sleep 0)
      (defun foo ()
        "dfdf"
        (declare (inline))
        (cond
          ((not (>= (length res) 10))
           (push t res)
           (error 'test-error))
          (t
           res))))

    (signals test-error
      (foo))
    (is (equal '(t t t)
                res))))

(test actually-do-automatic-retries-with-fake-sleep-fn-happy-path
  (let ((res nil))
    (with-auto-restart (:retries 3 :sleep (lambda (attempt) 0))
      (defun foo ()
        "dfdf"
        (declare (inline))
        (cond
          ((not (>= (length res) 10))
           (push t res)
           (error 'test-error))
          (t
           res))))

    (signals test-error
      (foo))
    (is (equal '(t t t)
                res))))


(test doc-and-declarations
  (with-auto-restart ()
    (defun foo (x)
      "hello world"
      (declare (optimize debug)
               ;; this next declare expr ensures this is
               ;; part of a function
               (inline))
      t))
  (is (equal "hello world" (documentation #'foo 'function))))

(test maybe-invoke-restart
  (let ((count 0)
        (seen nil))
    (with-auto-restart (:attempt attempt)
      (defun foo ()
        (push attempt seen)
        (incf count)
        (assert (< count 10))
        (when (< attempt 3)
          (invoke-restart 'retry-foo))))
    (foo)
    (is (eql 3 count))
    (is (equal (list 3 2 1) seen))))

(define-condition my-error (error)
  ())

(with-auto-restart (:retries 1000 :sleep 0)
  (defun crashes-after-many-attempts ()
    (error 'my-error)))

(test tail-call-optimized
  (signals my-error
    (crashes-after-many-attempts)))


(with-auto-restart (:attempt attempt)
  (defun nested-inner ()
    (unless (= attempt 1)
      (error "inner function didn't reset attempts, got ~a" attempt))))

(with-auto-restart (:attempt attempt)
  (defun nested-outer ()
    (assert (= attempt 1))
    (nested-inner)))

(test nested-calls-maintains-attempts
  (finishes
    (nested-outer)))

(with-auto-restart () ;; this auto-restart is important for the test
                      ;; we're trying to test.
 (defun nested-inner-with (counter)
   (unless (= counter 3)
     (error "inner function still failing, got ~a" counter))))

(with-auto-restart (:retries 3 :attempt attempt :sleep 0)
  (defun nested-outer-with-inner-failures ()
    (nested-inner-with attempt)
    attempt))

(test nested-calls-maintains-attempts
  (is (eql
       3
       (nested-outer-with-inner-failures))))

(defvar *nested-inner-2-counter*)

(with-auto-restart (:retries 5 :sleep 0) ;; this auto-restart is important for the test
                      ;; we're trying to test.
  (defun nested-inner-with-2 (counter)
    (incf *nested-inner-2-counter*)
    (unless (= counter 3)
      (error "inner function still failing, got ~a" counter))))

(with-auto-restart (:retries 3 :attempt attempt :sleep 0)
  (defun nested-outer-with-inner-failures-2 ()
    (nested-inner-with-2 attempt)
    attempt))

(test nested-calls-maintains-attempts-2
  (let ((*nested-inner-2-counter* 0))
    (is (eql
         3
         (nested-outer-with-inner-failures-2)))
    (is (eql
         11 *nested-inner-2-counter*))))

(define-condition dummy-error (error)
  ())

(with-auto-restart (:retries 3 :sleep 0 :auto-restartable-errors '(dummy-error))
  (defun call-with-crash (fn)
    (funcall fn)))

(test specify-auto-restartable-errors
  (let ((count 0))
    (signals dummy-error
     (call-with-crash
      (lambda ()
        (incf count)
        (error 'dummy-error))))
    (is (eql 3 count))))

(test ignore-errors-that-are-not-specified-as-auto-restartable
  (let ((count 0))
    (signals error
      (call-with-crash
       (lambda ()
         (incf count)
         (error 'error))))
    (is (eql 1 count))))

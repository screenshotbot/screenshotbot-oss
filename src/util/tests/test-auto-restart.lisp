;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-auto-restart
  (:use #:cl
        #:fiveam)
  (:import-from #:util/auto-restart
                #:restart-already-defined
                #:with-auto-restart)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-auto-restart)


(util/fiveam:def-suite)

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
    (with-auto-restart (:retries 3
                        )
      (defun foo ()
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

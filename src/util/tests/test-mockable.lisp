(defpackage :test-mockable
  (:use :cl
        :mockable
        :fiveam)
  (:export))
(in-package :test-mockable)

(defmockable func ()
  1)

(test without-mocks
  (is (equal 1 (func))))

(test with-mocks
  (flet ((make-call () (func)))
    (with-single-mock (func (lambda () 2))
      (is (equal 2 (make-call))))))

(test directly
  (with-single-mock (func (lambda () 2))
    (is (equal 2 (func)))))

(defmockable single-arg (str)
  (length str))

(test single-arg
  (is (equal 3 (single-arg "foo")))
  (with-single-mock (single-arg (lambda (str) (* 2 (length str))))
    (is (equal 6 (single-arg "foo")))))

(defmockable two-args (a b)
  (+ (length a) (length b)))

(test two-args
  (is (equal 5 (two-args "foo" "ba")))
  (with-single-mock (two-args (lambda (a b) (+ (* 2 (length a)) (length b))))
    (is (equal 8 (two-args "foo" "ba")))))

(defmockable key-method (a &key b)
  (+ (length a) (length b)))

(test key-method
  (is (equal 5 (key-method "foo" :b "ba")))
  (with-single-mock (key-method (lambda (a &key b) (+ (* 2 (length a)) (length b))))
    (is (equal 8 (key-method "foo" :b "ba")))))

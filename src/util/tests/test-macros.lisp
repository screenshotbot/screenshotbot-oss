;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-macros
  (:use #:cl
        #:util/macros
        #:fiveam)
  (:import-from #:util/macros
                #:unsupported-lambda-list
                #:get-non-bindings
                #:get-bindings
                #:binding-sym
                #:remove-&fn)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains))
(in-package :util/tests/test-macros)


(util/fiveam:def-suite)


(test preconditions
  (defblock with-basic-stuff (&fn fn)
    (funcall fn))

  (defblock with-return-something-else (&fn fn)
    (funcall fn)
    :another)
  (is (equal :test
             (with-basic-stuff ()
               :test)))
  (is (equal :another
             (with-return-something-else ()
               :test))))

(test can-use-arguments
  (defblock with-arg (add &fn fn)
    (+ add (funcall fn)))

  (is (equal 5 (with-arg (1)
                 4)))

  (let ((value 45))
    (is (equal 50 (with-arg (4)
                    (+ 1 value))))))

(test arguments-get-evaluated
  (defblock with-eval-arg (add &fn fn)
    (+ add (funcall fn)))

  (let ((value 1))
    (is (equal 5 (with-eval-arg (value)
                   4)))))

(test multiple-arguments
  (defblock with-multiple-args (one two &fn fn)
    (list one two))

  (is (equal (list :one :two)
             (with-multiple-args (:one :two)
               nil))))

(test remove-&fn
  (is (equal '(one two) (remove-&fn '(one two))))
  (is (equal '(one two) (remove-&fn '(one &fn fn two))))

  (assert-that (remove-&fn '(one &binding two))
               (contains
                'one
                 (has-typep 'binding-sym))))

(test get-bindings
  (is (equal '(aaa) (get-bindings
                   '(&binding one)
                    '(aaa))))
  (is (equal '(aaa) (get-bindings
                     '(&binding one &key two)
                      '(aaa))))
  (is (equal '() (get-bindings
                  '(one &key two)
                   '(aaa :two 2)))))

(test get-non-bindings
  (is (equal '() (get-non-bindings
                  '(&binding one)
                   '(aaa))))
  (is (equal '(2) (get-non-bindings
                   '(&binding one two)
                    '(aaa 2))))
  (is (equal '(:foo 2) (get-non-bindings
                        '(&binding one &key foo)
                         '(aaa :foo 2)))))

(test bindings
  (defblock with-bindings (&binding a &binding b &key one two
                                          &fn fn)
    (funcall fn 1 2))
  (is (equal 3
             (with-bindings (aaa bbb)
               (+ aaa bbb))))
  (signals unsupported-lambda-list
      (defblock with-key-bindings (&binding a &key &binding b)
        (funcall fn 1 3))))

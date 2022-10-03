;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-macros
  (:use #:cl
        #:easy-macros
        #:fiveam)
  (:import-from #:easy-macros
                #:unsupported-lambda-list
                #:get-non-bindings
                #:get-bindings
                #:binding-sym
                #:remove-&fn)
  (:import-from #:fiveam-matchers/core
                #:equal-to
                #:has-all
                #:is-not
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains))
(in-package :util/tests/test-macros)


(util/fiveam:def-suite)

(def-easy-macro with-basic-stuff (&fn fn)
  (funcall fn))

(def-easy-macro with-return-something-else (&fn fn)
  (funcall fn)
  :another)

(test preconditions
  (is (equal :test
             (with-basic-stuff ()
               :test)))
  (is (equal :another
             (with-return-something-else ()
               :test))))

(def-easy-macro with-arg (add &fn fn)
  (+ add (funcall fn)))

(test can-use-arguments

  (is (equal 5 (with-arg (1)
                 4)))

  (let ((value 45))
    (is (equal 50 (with-arg (4)
                    (+ 1 value))))))

(def-easy-macro with-eval-arg (add &fn fn)
  (+ add (funcall fn)))

(test arguments-get-evaluated
  (let ((value 1))
    (is (equal 5 (with-eval-arg (value)
                   4)))))

(def-easy-macro with-multiple-args (one two &fn fn)
  (list one two))

(test multiple-arguments
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

(test get-bindings-for-keys
  ;; Not that this would say &key foo, since this is the expression
  ;; that goes into the lamba-list for the user defined block
  (is (equal '(var)
              (get-bindings
               '(&key &binding foo)
                '(:foo var))))
  (is (equal '(aaa bbb) (get-bindings
                              '(&binding aaa &key &binding bbb)
                               '(aaa :bbb bbb)))))

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

(test get-non-bindings-for-keys
  (is (equal '() (get-non-bindings
                  '(&key &binding foo)
                   '(:foo var))))
  (is (equal '() (get-non-bindings
                  '(&binding aaa &key &binding bbb)
                   '(aaa :bbb bb))))
  (is (equal '(:foo 2)
              (get-non-bindings
               '(&binding aaa &key &binding bbb foo)
                '(aaa :bbb bbb :foo 2)))))

(def-easy-macro with-bindings (&binding a &binding b &key one two
                                  &fn fn)
  (funcall fn 1 2))


(test bindings
  (is (equal 3
             (with-bindings (aaa bbb)
               (+ aaa bbb))))
  #+nil
  (signals unsupported-lambda-list
    (eval
     `(def-easy-macro with-key-bindings (&binding a &key &binding b)
        (funcall fn 1 3)))))


(def-easy-macro with-key-bindings (&binding a &key &binding b one two
                                            &fn fn)
  (funcall fn 1 2))


(test bindings-with-keys
  (is (equal 3
             (with-key-bindings (aaa :b bbb)
               (+ aaa bbb)))))

(def-easy-macro collect-loop (&binding item list &fn fn)
  (loop for x in list
        for i from 0
        collect (funcall fn x)))

(test default-binding-example
  (is
   (equal '(2 4 6)
    (collect-loop (item '(1 2 3))
      (* 2 item)))))

(def-easy-macro collect-loop-with-index (&binding item list &key &binding index &fn fn)
  (loop for x in list
        for i from 0
        collect (funcall fn x i)))

(test default-binding-example-with-index
  (is
   (equal '(0 2 6)
    (collect-loop-with-index (item '(1 2 3) :index i)
      (* item i))))
  (is
   (equal '(1 2 3)
    (collect-loop-with-index (item '(1 2 3))
      item))))

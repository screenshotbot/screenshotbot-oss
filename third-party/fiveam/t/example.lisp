;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; * FiveAM Example (poor man's tutorial)

(asdf:oos 'asdf:load-op :fiveam)

(defpackage :it.bese.fiveam.example
  (:use :common-lisp
	:it.bese.fiveam))

(in-package :it.bese.fiveam.example)

;;;; First we need some functions to test.

(defun add-2 (n)
  (+ n 2))

(defun add-4 (n) 
  (+ n 4))

;;;; Now we need to create a test which makes sure that add-2 and add-4
;;;; work as specified.

;;;; we create a test named ADD-2 and supply a short description.
(test add-2
 "Test the ADD-2 function" ;; a short description
 ;; the checks
 (is (= 2 (add-2 0)))
 (is (= 0 (add-2 -2))))

;;;; we can already run add-2. This will return the list of test
;;;; results, it should be a list of two test-passed objects.

(run 'add-2) 

;;;; since we'd like to have some kind of readbale output we'll explain
;;;; the results

(explain! (run 'add-2))

;;;; or we could do both at once:

(run! 'add-2)

;;;; So now we've defined and run a single test. Since we plan on
;;;; having more than one test and we'd like to run them together let's
;;;; create a simple test suite.

(def-suite example-suite :description "The example test suite.")

;;;; we could explictly specify that every test we create is in the the
;;;; example-suite suite, but it's easier to just change the default
;;;; suite:

(in-suite example-suite)

;;;; now we'll create a new test for the add-4 function.

(test add-4
  (is (= 0 (add-4 -4))))

;;;; now let's run the test

(run! 'add-4)

;;;; we can get the same effect by running the suite:

(run! 'example-suite)

;;;; since we'd like both add-2 and add-4 to be in the same suite, let's
;;;; redefine add-2 to be in this suite:

(test add-2 "Test the ADD-2 function"
 (is (= 2 (add-2 0)))
 (is (= 0 (add-2 -2))))

;;;; now we can run the suite and we'll see that both add-2 and add-4
;;;; have been run (we know this since we no get 4 checks as opposed to
;;;; 2 as before.

(run! 'example-suite)

;;;; Just for fun let's see what happens when a test fails. Again we'll
;;;; redefine add-2, but add in a third, failing, check:

(test add-2 "Test the ADD-2 function"
 (is (= 2 (add-2 0)))
 (is (= 0 (add-2 -2)))
 (is (= 0 (add-2 0))))

;;;; Finally let's try out the specification based testing.

(defun dummy-add (a b)
  (+ a b))

(defun dummy-strcat (a b)
  (concatenate 'string a b))

(test dummy-add
  (for-all ((a (gen-integer))
            (b (gen-integer)))
    ;; assuming we have an "oracle" to compare our function results to
    ;; we can use it:
    (is (= (+ a b) (dummy-add a b)))
    ;; if we don't have an oracle (as in most cases) we just ensure
    ;; that certain properties hold:
    (is (= (dummy-add a b)
           (dummy-add b a)))
    (is (= a (dummy-add a 0)))
    (is (= 0 (dummy-add a (- a))))
    (is (< a (dummy-add a 1)))
    (is (= (* 2 a) (dummy-add a a)))))

(test dummy-strcat
  (for-all ((result (gen-string))
            (split-point (gen-integer :min 0 :max 10000)
                         (< split-point (length result))))
    (is (string= result (dummy-strcat (subseq result 0 split-point)
                                      (subseq result split-point))))))

(test random-failure
  (for-all ((result (gen-integer :min 0 :max 1)))
    (is (plusp result))
    (is (= result 0))))

(run! 'example-suite)

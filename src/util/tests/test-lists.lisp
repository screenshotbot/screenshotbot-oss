;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-lists
  (:use #:cl
        #:util/lists
        #:fiveam)
  (:import-from #:util/lists
                #:with-batches
                #:make-batches)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-lists)

(util/fiveam:def-suite)

(test head
  (is (equal '(1 2) (head '(1 2 3) 2)))
  (is (equal nil (head '(1 2 3) 0)))
  (is (equal '(1 2 3) (head '(1 2 3) 3)))
  (is (equal '(1 2 3) (head '(1 2 3) 5))))

(test head-negative
  (is (equal '(1 2) (head '(1 2 3) -1)))
  (is (equal '(1 2 3) (head '(1 2 3) -10)))
  (is (equal '(1) (head '(1 2 3) -2))))

(test tail
  (is (equal '(3) (tail '(1 2 3) 2)))
  (is (equal nil (tail '(1 2 3) 3)))
  (is (equal '(1 2 3) (tail '(1 2 3) 0)))
  (is (equal nil (tail '(1 2 3) 5))))


(test tail-negative
  (is (equal '(3) (tail '(1 2 3) -1)))
  (is (equal '(2 3) (tail '(1 2 3) -2)))
  (is (equal '(1 2 3) (tail '(1 2 3) -3)))
  (is (equal '(1 2 3) (tail '(1 2 3) -10))))


(test make-batches
  (is (equal '((1 2 3)
               (4 5 6)
               (7 8))
             (make-batches '(1 2 3 4 5 6 7 8)
                           :batch-size 3)))
  (is (equal '(0 3 6)
             (nth-value
              1
              (make-batches '(1 2 3 4 5 6 7 8)
                            :batch-size 3)))))

(test with-batches-is-tail-call-optimized
  (let ((list (loop for i from 0 below 1000000
                    collect i)))
    (let ((num 0))
      (with-batches (batch list)
        (incf num))
      (is (eql 100000 num)))))

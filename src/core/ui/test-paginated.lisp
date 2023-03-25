;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/test-paginated
  (:use #:cl
        #:fiveam)
  (:import-from #:core/ui/paginated
                #:pagination-helper)
  (:shadowing-import-from #:new-let
                          #:let)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:equal-to
                #:is-equal-to
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains))
(in-package :core/ui/test-paginated)

(util/fiveam:def-suite)

(defun %pagination-helper (&rest args)
  "A version of pagination helper that instead of calling the renderer,
just returns three values: the list of objects and a lambda to return
the next list of objects and the start-index"
  (apply #'pagination-helper
         :renderer (lambda (objects next start-counter)
                     (values objects next start-counter))
         :empty-view :empty
         args))

(defun make-test-list (num)
  (loop for i from 0 below num
        collect (format nil "~a" i)))

(defun make-test-map (num)
  (fset:convert
   'fset:map
   (loop for i from 0 below num
         collect (cons
                  (format nil "~a" i)
                  (+ 100 i)))))

(test empty-view
  (is (eql :empty (%pagination-helper :num 5
                                      :items (make-test-list 0))))
  (is (not (eql :empty (%pagination-helper :num 5
                                           :items (make-test-list 3)))))
  (is (not (eql :empty (%pagination-helper :num 5
                                           :items (make-test-list 13)))))
  (is (eql :empty (%pagination-helper :num 5
                                      :filter (lambda (x) nil)
                                      :items (make-test-list 13))))
  (assert-that (%pagination-helper :num 5
                                   :filter (lambda (x)
                                             (string= x "3"))
                                   :items (make-test-list 13))
               (contains
                (equal-to "3"))))

(test more-link
  (let ((items more (%pagination-helper :num 3
                                        :items (make-test-list 7))))
    (assert-that items
                 (contains "0" "1" "2"))
    (let ((items more (funcall more)))
      (assert-that items (contains "3" "4" "5"))
      (let ((items more (funcall more)))
        (assert-that items (contains "6"))
        (is (null more))))))

(test more-link-with-filter
  (let ((items more (%pagination-helper :num 3
                                        :filter (lambda (x)
                                                  (< (parse-integer x) 7))
                                        :items (make-test-list 10))))
    (assert-that items
                 (contains "0" "1" "2"))
    (let ((items more (funcall more)))
      (assert-that items (contains "3" "4" "5"))
      (let ((items more (funcall more)))
        (assert-that items (contains "6"))
        (is (null more))))))

(test more-link-with-maps
  (let ((items more (%pagination-helper :num 3
                                        :items (make-test-map 7))))
    (assert-that items
                 (contains (cons "0" 100) (cons "1" 101) (cons "2" 102)))
    (let ((items more (funcall more)))
      (assert-that items (contains (cons "3" 103) (cons "4" 104) (cons "5" 105)))
      (let ((items more (funcall more)))
        (assert-that items (contains (cons "6" 106)))
        (is (null more))))))

(test more-link-with-filtered-maps
  (let ((items more (%pagination-helper :num 3
                                        :filter (lambda (x)
                                                  (< (parse-integer x) 7))
                                        :items (make-test-map 10))))
    (assert-that items
                 (contains (cons "0" 100) (cons "1" 101) (cons "2" 102)))
    (let ((items more (funcall more)))
      (assert-that items (contains (cons "3" 103) (cons "4" 104) (cons "5" 105)))
      (let ((items more (funcall more)))
        (assert-that items (contains (cons "6" 106)))
        (is (null more))))))

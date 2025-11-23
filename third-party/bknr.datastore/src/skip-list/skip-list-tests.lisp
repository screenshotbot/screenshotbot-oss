(defpackage :bknr.skip-list-tests
  (:use #:cl
        #:fiveam
        #:bknr.skip-list)
  (:import-from #:bknr.skip-list
                #:make-node
                #:node-level
                #:node-forward
                #:skip-list-to-list))
(in-package :bknr.skip-list-tests)

(def-suite* :bknr.skip-list)

(defmacro define-skip-list-test (name &rest body)
  `(test ,(intern (string name))
     ,@body))

(defmacro test-equal (&rest args)
  `(is (equal ,@args)))

(defmacro test-assert (&rest args)
  `(is-true ,@args))

(defmacro test-condition (expr (%quote condition))
  (declare (ignore %quote))
  `(signals ,condition
     ,expr))

(define-skip-list-test "Node test"
    (let ((node (make-node 0 0 23)))
      (dotimes (i (node-level node))
	(test-equal (node-forward node i) nil))
      (dotimes (i (node-level node))
	(setf (node-forward node i) i))
      (dotimes (i (node-level node))
	(test-equal (node-forward node i) i))))

(define-skip-list-test "Skiplist test"
    (let ((sl (make-instance 'skip-list)))
      (dotimes (i 10000)
	(let ((value (random 10000))
	      (key (random 10000)))
	  (skip-list-insert sl key value)
      ;; Avoiding fiveam here for the noise
	  (assert (equal (skip-list-search sl key) value)))))
  (pass))

(define-skip-list-test "Hashcompare"
    (let ((sl (make-instance 'skip-list))
	  (hash (make-hash-table)))
      (dotimes (i 10000)
	(let ((value (random 10000))
	      (key (random 10000)))
	  (setf (gethash key hash) value)
	  (skip-list-insert sl key value)))
      (loop for key being the hash-key of hash
	    do (assert (equal (gethash key hash)
                          (skip-list-search sl key)))))
  (pass))

(define-skip-list-test "Length of skiplist"
    (let ((sl (make-instance 'skip-list)))
      (dotimes (i 100)
	    (skip-list-insert sl i i)
	    (test-equal (skip-list-search sl i) i)
	    (test-equal (skip-list-search sl (1+ i) :not-found) :not-found)
	    (dotimes (j i)
	      (assert (equal (skip-list-search sl j) j)))
	    (test-equal (skip-list-length sl) (1+ i)))
      (dotimes (i 100)
	    (test-equal (skip-list-length sl) (- 100 i))
	    (skip-list-delete sl i)
	    (test-equal (skip-list-length sl) (- 100 (1+ i)))
	    (test-equal (skip-list-search sl i :not-found) :not-found)
	    (loop for j from (1+ i) to 99
	          do (assert (equal (skip-list-search sl j) j))))
      (test-equal (skip-list-length sl) 0)
      (test-equal (skip-list-to-list sl) nil)))


;; (defun perf ()
;;   (let ((sl (make-instance 'skip-list)))
;;     (time (prof:with-profiling ()
;; 	    (dotimes (i 100000)
;; 	      (skip-list-insert sl i i))))
;;     (prof:show-flat-profile)
;;     (format t "~%~%")
;;     (time (prof:with-profiling ()
;; 	    (dotimes (i 100000)
;; 	      (skip-list-search sl i))))
;;     (prof:show-flat-profile)))

;;; Cursor tests

(define-skip-list-test "Basic cursor - empty list"
    (let* ((sl (make-instance 'skip-list))
           (cursor (skip-list-cursor sl)))
      (test-equal (sl-cursor-next cursor) nil)))

(define-skip-list-test "Basic cursor - single element"
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 1 :one)
      (let ((cursor (skip-list-cursor sl)))
        (test-equal (sl-cursor-next cursor) '(1 :one))
        (test-equal (sl-cursor-next cursor) nil))))

(define-skip-list-test "Basic cursor - multiple elements"
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 2 :two)
      (skip-list-insert sl 3 :three)
      (let ((cursor (skip-list-cursor sl)))
        (test-equal (sl-cursor-next cursor) '(1 :one))
        (test-equal (sl-cursor-next cursor) '(2 :two))
        (test-equal (sl-cursor-next cursor) '(3 :three))
        (test-equal (sl-cursor-next cursor) nil)
        (test-equal (sl-cursor-next cursor) nil)))) ; Multiple calls after end return nil

(define-skip-list-test "Basic cursor - sorted order"
    (let* ((sl (make-instance 'skip-list)))
      ;; Insert in random order
      (skip-list-insert sl 5 :five)
      (skip-list-insert sl 2 :two)
      (skip-list-insert sl 8 :eight)
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 3 :three)

      ;; Cursor should return in sorted key order
      (let ((cursor (skip-list-cursor sl)))
        (test-equal (sl-cursor-next cursor) '(1 :one))
        (test-equal (sl-cursor-next cursor) '(2 :two))
        (test-equal (sl-cursor-next cursor) '(3 :three))
        (test-equal (sl-cursor-next cursor) '(5 :five))
        (test-equal (sl-cursor-next cursor) '(8 :eight))
        (test-equal (sl-cursor-next cursor) nil))))

(define-skip-list-test "Cursor prev - not supported"
    (let* ((sl (make-instance 'skip-list))
           (cursor (skip-list-cursor sl)))
      (test-condition (sl-cursor-prev cursor) 'error)))

(define-skip-list-test "Keys cursor - returns only keys"
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 2 :two)
      (skip-list-insert sl 3 :three)

      (let ((cursor (skip-list-keys-cursor sl)))
        (test-equal (sl-cursor-next cursor) 1)
        (test-equal (sl-cursor-next cursor) 2)
        (test-equal (sl-cursor-next cursor) 3)
        (test-equal (sl-cursor-next cursor) nil))))

(define-skip-list-test "Values cursor - returns only values"
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 2 :two)
      (skip-list-insert sl 3 :three)

      (let ((cursor (skip-list-values-cursor sl)))
        (test-equal (sl-cursor-next cursor) :one)
        (test-equal (sl-cursor-next cursor) :two)
        (test-equal (sl-cursor-next cursor) :three)
        (test-equal (sl-cursor-next cursor) nil))))

(define-skip-list-test "Range cursor - basic range"
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 2 :two)
      (skip-list-insert sl 3 :three)
      (skip-list-insert sl 4 :four)
      (skip-list-insert sl 5 :five)

      ;; Range [2, 4) should return 2 and 3
      (let ((cursor (skip-list-range-cursor sl 2 4)))
        (test-equal (sl-cursor-next cursor) '(2 :two))
        (test-equal (sl-cursor-next cursor) '(3 :three))
        (test-equal (sl-cursor-next cursor) nil))))

(define-skip-list-test "Range cursor - start before first"
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 5 :five)
      (skip-list-insert sl 10 :ten)

      ;; Range [0, 7) should return only 5
      (let ((cursor (skip-list-range-cursor sl 0 7)))
        (test-equal (sl-cursor-next cursor) '(5 :five))
        (test-equal (sl-cursor-next cursor) nil))))

(define-skip-list-test "Range cursor - end after last"
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 5 :five)
      (skip-list-insert sl 10 :ten)

      ;; Range [5, 100) should return both
      (let ((cursor (skip-list-range-cursor sl 5 100)))
        (test-equal (sl-cursor-next cursor) '(5 :five))
        (test-equal (sl-cursor-next cursor) '(10 :ten))
        (test-equal (sl-cursor-next cursor) nil))))

(define-skip-list-test "Range cursor - empty range"
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 5 :five)

      ;; Range [2, 4) has no elements
      (let ((cursor (skip-list-range-cursor sl 2 4)))
        (test-equal (sl-cursor-next cursor) nil))))

(define-skip-list-test "Range cursor - no matching start"
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 2 :two)

      ;; Range [10, 20) has no elements
      (let ((cursor (skip-list-range-cursor sl 10 20)))
        (test-equal cursor nil))))

(define-skip-list-test "map-skip-list - basic iteration"
    (let* ((sl (make-instance 'skip-list))
           (results nil))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 2 :two)
      (skip-list-insert sl 3 :three)

      (map-skip-list (lambda (key val)
                       (push (list key val) results))
                     sl)

      ;; Results should be in reverse order (due to push)
      (test-equal (nreverse results) '((1 :one) (2 :two) (3 :three)))))

(define-skip-list-test "map-skip-list - empty list"
    (let* ((sl (make-instance 'skip-list))
           (count 0))
      (map-skip-list (lambda (key val)
                       (declare (ignore key val))
                       (incf count))
                     sl)
      (test-equal count 0)))

(define-skip-list-test "map-skip-list-values - returns only values"
    (let* ((sl (make-instance 'skip-list))
           (results nil))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 2 :two)
      (skip-list-insert sl 3 :three)

      (map-skip-list-values (lambda (val)
                              (push val results))
                            sl)

      (test-equal (nreverse results) '(:one :two :three))))

(define-skip-list-test "Cursor independence - multiple cursors"
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 2 :two)
      (skip-list-insert sl 3 :three)

      (let ((cursor1 (skip-list-cursor sl))
            (cursor2 (skip-list-cursor sl)))
        ;; Advance cursor1 twice
        (sl-cursor-next cursor1)
        (sl-cursor-next cursor1)

        ;; cursor2 should still be at the beginning
        (test-equal (sl-cursor-next cursor2) '(1 :one))

        ;; cursor1 should be at position 3
        (test-equal (sl-cursor-next cursor1) '(3 :three)))))

(define-skip-list-test "Cursor with EOC marker"
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 1 :one)

      (let ((cursor (skip-list-cursor sl)))
        (test-equal (sl-cursor-next cursor) '(1 :one))
        (test-equal (sl-cursor-next cursor :eoc) :eoc)
        (test-equal (sl-cursor-next cursor :end-of-list) :end-of-list))))

(define-skip-list-test "Cursor result list identity - mutation test"
    ;; This test verifies current behavior: each call returns the same
    ;; list object. This test might a bit brittle to implementation
    ;; detail though.
    (let* ((sl (make-instance 'skip-list)))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 2 :two)

      (let ((cursor (skip-list-cursor sl)))
        (let ((result1 (sl-cursor-next cursor))
              (result2 (sl-cursor-next cursor)))

          ;; Currently, results are identical cons cells
          (test-assert (eq result1 result2))))))

(define-skip-list-test "Cursor result storage - safety test"
    ;; This test verifies that storing cursor results works correctly
    ;; REQUIRES copy-list because the same list object is reused
    (let* ((sl (make-instance 'skip-list))
           (stored-results nil))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 2 :two)
      (skip-list-insert sl 3 :three)

      (let ((cursor (skip-list-cursor sl)))
        (loop for result = (sl-cursor-next cursor)
              while result
              do (push (copy-list result) stored-results)))

      ;; All stored results should maintain their original values
      (test-equal (length stored-results) 3)
      (test-equal (first (third stored-results)) 1)
      (test-equal (second (third stored-results)) :one)
      (test-equal (first (second stored-results)) 2)
      (test-equal (second (second stored-results)) :two)
      (test-equal (first (first stored-results)) 3)
      (test-equal (second (first stored-results)) :three)))

(define-skip-list-test "Cursor result storage - WITHOUT copy demonstrates the problem"
    ;; This test demonstrates what happens if you DON'T copy the result
    ;; All stored references point to the same list with the FINAL values
    (let* ((sl (make-instance 'skip-list))
           (stored-results nil))
      (skip-list-insert sl 1 :one)
      (skip-list-insert sl 2 :two)
      (skip-list-insert sl 3 :three)

      ;; Store WITHOUT copying - this is WRONG but demonstrates the behavior
      (let ((cursor (skip-list-cursor sl)))
        (loop for result = (sl-cursor-next cursor)
              while result
              do (push result stored-results)))

      ;; All three stored "results" point to the SAME list object
      (test-equal (length stored-results) 3)
      (test-assert (eq (first stored-results) (second stored-results)))
      (test-assert (eq (second stored-results) (third stored-results)))

      ;; And they all contain the FINAL values (3 :three)
      (test-equal (first (first stored-results)) 3)
      (test-equal (second (first stored-results)) :three)
      (test-equal (first (second stored-results)) 3)
      (test-equal (second (second stored-results)) :three)
      (test-equal (first (third stored-results)) 3)
      (test-equal (second (third stored-results)) :three)))

(define-skip-list-test "Large list cursor iteration"
    (let* ((sl (make-instance 'skip-list))
           (count 0))
      ;; Insert 1000 elements
      (dotimes (i 1000)
        (skip-list-insert sl i (* i 10)))

      ;; Verify cursor iterates over all elements in order
      (let ((cursor (skip-list-cursor sl)))
        (loop for result = (sl-cursor-next cursor)
              while result
              do (progn
                   (test-equal (first result) count)
                   (test-equal (second result) (* count 10))
                   (incf count))))

      (test-equal count 1000)))

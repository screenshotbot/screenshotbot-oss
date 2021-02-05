;;;; damn-fast-stable-priority-queue-test-same.lisp

(defpackage #:damn-fast-stable-priority-queue/test-same-priorities
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:q #:damn-fast-stable-priority-queue))
  (:export #:run))

(in-package #:damn-fast-stable-priority-queue/test-same-priorities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utilities

(defun verify-heap-property (vector)
  (loop with length = (length vector)
        for parent from 0 below (truncate length 2)
        for left = (+ (* parent 2) 1)
        for right = (+ (* parent 2) 2)
        do (assert (< (aref vector parent) (aref vector left)) ()
                   "VERIFY-HEAP-PROPERTY: Invalid left child: ~D -> ~D"
                   (aref vector parent) (aref vector left))
        when (oddp length)
          do (assert (< (aref vector parent) (aref vector right)) ()
                     "VERIFY-HEAP-PROPERTY: Invalid right child: ~D -> ~D"
                     (aref vector parent) (aref vector right))))

(defun stringify (i) (format nil "~D" i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Main interface

(defvar *verbose* nil)

(defun run (&optional *verbose*)
  (dolist (length (nconc (a:iota 64 :start 1) '(256 1024 4096)))
    (when *verbose* (format t "~&Testing with ~4,' D elements" length))
    (let ((queue (q:make-queue (max 1 (ash length -4)))))
      (perform-test queue (a:iota length))
      (perform-test queue (nreverse (a:iota length)))
      (dotimes (i 100)
        (perform-test queue (a:shuffle (a:iota length)))))))

(defun perform-test (queue list)
  (when *verbose* (princ "."))
  (test-enqueue queue list)
  (test-map queue list)
  (test-do-queue queue list)
  (test-dequeue-and-peek queue list)
  (test-dequeue-and-peek-empty queue)
  (test-trim queue list)
  (setf (q::%count queue) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Subtests

(defun test-enqueue (queue list)
  (let ((counter 0))
    (dolist (i list)
      (q:enqueue queue (stringify i) 0)
      (assert (= (incf counter) (q:size queue)))
      (verify-heap-property (subseq (q::%count-vector queue)
                                    0 (q:size queue))))))

(defun test-map (queue list)
  (let ((expected (reduce #'+ list))
        (actual 0))
    (q:map queue (lambda (x) (incf actual (parse-integer x))))
    (assert (= expected actual))))

(defun test-do-queue (queue list)
  (let ((expected (reduce #'+ list))
        (actual 0))
    (q:do-queue (x queue) (incf actual (parse-integer x)))
    (assert (= expected actual))))

(defun test-dequeue (queue expected-value expected-foundp)
  (multiple-value-bind (value foundp) (q:dequeue queue)
    (assert (equal expected-value value))
    (assert (eql expected-foundp foundp))))

(defun test-peek (queue expected-value expected-foundp)
  (multiple-value-bind (value foundp) (q:peek queue)
    (assert (equal expected-value value))
    (assert (eql expected-foundp foundp))))

(defun test-dequeue-and-peek (queue list)
  (let ((counter (q:size queue)))
    (loop for i from 0
          for elt in list
          do (test-peek queue (stringify elt) t)
             (assert (= counter (q:size queue)))
             (test-dequeue queue (stringify elt) t)
             (assert (= (decf counter) (q:size queue))))))

(defun test-dequeue-and-peek-empty (queue)
  (test-peek queue nil nil)
  (assert (= 0 (q:size queue)))
  (test-dequeue queue nil nil)
  (assert (= 0 (q:size queue))))

(defun test-trim (queue list)
  (assert (<= (length list) (length (q::%prio-vector queue))))
  (assert (<= (length list) (length (q::%data-vector queue))))
  (assert (<= (length list) (length (q::%count-vector queue))))
  (q:trim queue)
  (assert (= 0 (length (q::%prio-vector queue))))
  (assert (= 0 (length (q::%data-vector queue))))
  (assert (= 0 (length (q::%count-vector queue)))))

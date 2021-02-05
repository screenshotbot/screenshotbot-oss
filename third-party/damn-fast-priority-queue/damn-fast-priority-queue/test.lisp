;;;; damn-fast-priority-queue-test.lisp

(defpackage #:damn-fast-priority-queue/test
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:q #:damn-fast-priority-queue))
  (:export #:run))

(in-package #:damn-fast-priority-queue/test)

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
        (perform-test queue (a:shuffle (a:iota length))))))
  (perform-error-test)
  (perform-copy-test))

(defun perform-test (queue list)
  (when *verbose* (princ "."))
  (test-enqueue queue list)
  (test-map queue list)
  (test-do-queue queue list)
  (test-dequeue-and-peek queue list)
  (test-dequeue-and-peek-empty queue)
  (test-trim queue list))

(defun perform-error-test ()
  (let ((queue (q:make-queue 4 2 nil)))
    (dotimes (i 4) (q:enqueue queue (princ-to-string i) i))
    (flet ((perform ()
             (multiple-value-bind (value error)
                 (ignore-errors (q:enqueue queue "4" 4))
               (assert (null value))
               (assert (typep error 'q:queue-size-limit-reached))
               (assert (eq queue (q:queue-size-limit-reached-queue error)))
               (assert (string= "4"
                                (q:queue-size-limit-reached-object error))))))
      (dotimes (i 4) (perform)))))

(defun perform-copy-test ()
  (let ((queue-1 (q:make-queue)))
    (q:enqueue queue-1 42 1)
    (let ((queue-2 (q:copy-queue queue-1)))
      (q:enqueue queue-2 24 0)
      ;; Check QUEUE-1
      (multiple-value-bind (value foundp) (q:dequeue queue-1)
        (assert (= 42 value))
        (assert (eq t foundp)))
      (multiple-value-bind (value foundp) (q:dequeue queue-1)
        (assert (null value))
        (assert (null foundp)))
      ;; Check QUEUE-2
      (multiple-value-bind (value foundp) (q:dequeue queue-2)
        (assert (= 24 value))
        (assert (eq t foundp)))
      (multiple-value-bind (value foundp) (q:dequeue queue-2)
        (assert (= 42 value))
        (assert (eq t foundp)))
      (multiple-value-bind (value foundp) (q:dequeue queue-2)
        (assert (null value))
        (assert (null foundp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Subtests

(defun test-enqueue (queue list)
  (let ((counter 0))
    (dolist (i list)
      (q:enqueue queue (stringify i) i)
      (assert (= (incf counter) (q:size queue)))
      (verify-heap-property (subseq (q::%prio-vector queue)
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
    (dotimes (i (length list))
      (test-peek queue (stringify i) t)
      (assert (= counter (q:size queue)))
      (test-dequeue queue (stringify i) t)
      (assert (= (decf counter) (q:size queue))))))

(defun test-dequeue-and-peek-empty (queue)
  (test-peek queue nil nil)
  (assert (= 0 (q:size queue)))
  (test-dequeue queue nil nil)
  (assert (= 0 (q:size queue))))

(defun test-trim (queue list)
  (assert (<= (length list) (length (q::%prio-vector queue))))
  (assert (<= (length list) (length (q::%data-vector queue))))
  (q:trim queue)
  (assert (= 0 (length (q::%prio-vector queue))))
  (assert (= 0 (length (q::%data-vector queue)))))

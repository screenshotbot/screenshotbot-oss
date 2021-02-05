;;;; damn-fast-stable-priority-queue-test.lisp

(defpackage #:damn-fast-stable-priority-queue/test
  (:use #:cl)
  (:local-nicknames (#:q #:damn-fast-stable-priority-queue))
  (:export #:run))

(in-package #:damn-fast-stable-priority-queue/test)

(defun run (&optional verbose)
  (when verbose (format t "~&;;; Testing with the same priorities."))
  (damn-fast-stable-priority-queue/test-same-priorities:run verbose)
  (when verbose (format t "~&;;; Testing with distinct priorities."))
  (damn-fast-stable-priority-queue/test-distinct-priorities:run verbose)
  (when verbose (format t "~&;;; Testing semantics."))
  (perform-error-test)
  (perform-copy-test)
  (when verbose (format t "~&;;; Test complete.")))

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

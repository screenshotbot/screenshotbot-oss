;;;; benchmark.lisp

(defpackage #:priority-queue-benchmark
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:run))

(in-package #:priority-queue-benchmark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Test parameters

(defconstant +capacity+ 409600)
(defconstant +repeat-count+ 10)
(defconstant +pass-capacity-p+ t)

(defparameter *test-vectors*
  '(:increasing :decreasing :shuffled :zero))

(defparameter *test-functions*
  (list 'test-pettomato-indexed-priority-queue
        'test-priority-queue
        'test-queues
        'test-pileup
        'test-bodge-heap
        'test-cl-heap
        'test-heap
        'test-minheap
        'test-damn-fast-priority-queue
        'test-damn-fast-stable-priority-queue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Performance testq

(defun perform-test (name vector-name vector
                     &key make-fn push-fn peek-fn pop-fn)
  (declare (type function make-fn push-fn peek-fn pop-fn))
  (declare (optimize speed))
  (let* ((queue (funcall make-fn)))
    (format t "~&;;; Library: ~A" name)
    (format t "~&;;; Element order: ~A~%~%" vector-name)
    (trivial-garbage:gc :full t)
    (time (dotimes (i +repeat-count+)
            (map nil (lambda (i) (funcall push-fn queue i)) vector)
            (if (eq vector-name :zero)
                (dotimes (i +capacity+)
                  (assert (= 0 (the fixnum (funcall peek-fn queue))))
                  (assert (= 0 (the fixnum (funcall pop-fn queue)))))
                (dotimes (i +capacity+)
                  (assert (= i (the fixnum (funcall peek-fn queue))))
                  (assert (= i (the fixnum (funcall pop-fn queue))))))))))

(defun make-test-vectors ()
  (declare (optimize speed))
  (let ((zero (make-array +capacity+
                          :element-type `(integer 0 ,(1- +capacity+))
                          :initial-element 0))
        (increasing (make-array +capacity+
                                :element-type `(integer 0 ,(1- +capacity+)))))
    (loop for i from 0 below +capacity+ do (setf (aref increasing i) i))
    (let ((decreasing (nreverse (copy-seq increasing)))
          (shuffled (a:shuffle (copy-seq increasing))))
      `(,(when (member :increasing *test-vectors*) `(:increasing ,increasing))
        ,(when (member :decreasing *test-vectors*) `(:decreasing ,decreasing))
        ,(when (member :shuffled *test-vectors*) `(:shuffled ,shuffled))
        ,(when (member :zero *test-vectors*) `(:zero ,zero))))))

(defun run ()
  (declare (optimize speed))
  (format t "~&;;; Starting a priority queue performance test.")
  (format t "~&;;; Testing with ~D elements and ~D repeats."
          +capacity+ +repeat-count+)
  (format t "~&;;; ~:[Not p~;P~]assing capacity to constructors.~%~%"
          +pass-capacity-p+)
  (dolist (data (make-test-vectors))
    (dolist (function *test-functions*)
      (apply (the function function) data)))
  (format t "~&;;; Performance test complete.~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implementations

(defun test-pettomato-indexed-priority-queue (vector-name vector)
  (declare (optimize speed))
  (perform-test
   :pettomato-indexed-priority-queue
   vector-name vector
   :make-fn (lambda ()
              (if +pass-capacity-p+
                  (let* ((hash (make-hash-table :test 'eql)))
                    (pettomato-indexed-priority-queue:make-empty-queue
                     #'<
                     (lambda (item index) (setf (gethash item hash) index))
                     (lambda (item) (gethash item hash -1))
                     :size +capacity+))
                  (let* ((hash (make-hash-table :test 'eql)))
                    (pettomato-indexed-priority-queue:make-empty-queue
                     #'<
                     (lambda (item index) (setf (gethash item hash) index))
                     (lambda (item) (gethash item hash -1))))))
   :push-fn (lambda (q i) (pettomato-indexed-priority-queue:queue-insert q i))
   :peek-fn (lambda (q) (pettomato-indexed-priority-queue:queue-peek q))
   :pop-fn (lambda (q) (pettomato-indexed-priority-queue:queue-pop q))))

(defun test-priority-queue (vector-name vector)
  (declare (optimize speed))
  (perform-test
   :priority-queue
   vector-name vector
   ;; No way to pass the starting capacity.
   :make-fn (lambda () (priority-queue:make-pqueue #'<))
   :push-fn (lambda (q i) (priority-queue:pqueue-push i i q))
   :peek-fn (lambda (q) (priority-queue:pqueue-front q))
   :pop-fn (lambda (q) (priority-queue:pqueue-pop q))))

(defun test-queues (vector-name vector)
  (declare (optimize speed))
  (perform-test
   :queues
   vector-name vector
   ;; No way to pass the starting capacity.
   :make-fn (lambda () (queues:make-queue :priority-queue))
   :push-fn (lambda (q i) (queues:qpush q i))
   :peek-fn (lambda (q) (queues:qtop q))
   :pop-fn (lambda (q) (queues:qpop q))))

(defun test-pileup (vector-name vector)
  (declare (optimize speed))
  (perform-test
   :pileup
   vector-name vector
   :make-fn (lambda () (if +pass-capacity-p+
                           (pileup:make-heap #'< :size +capacity+)
                           (pileup:make-heap #'<)))
   :push-fn (lambda (q i) (pileup:heap-insert i q))
   :peek-fn (lambda (q) (pileup:heap-top q))
   :pop-fn (lambda (q) (pileup:heap-pop q))))

(defun test-bodge-heap (vector-name vector)
  (declare (optimize speed))
  (perform-test
   :bodge-heap
   vector-name vector
   :make-fn (lambda () (if +pass-capacity-p+
                           (bodge-heap:make-binary-heap
                            :expansion-factor +capacity+)
                           (bodge-heap:make-binary-heap)))
   :push-fn (lambda (q i) (bodge-heap:binary-heap-push q i))
   :peek-fn (lambda (q) (bodge-heap:binary-heap-peek q))
   :pop-fn (lambda (q) (bodge-heap:binary-heap-pop q))))

(defun test-cl-heap (vector-name vector)
  (declare (optimize speed))
  (perform-test
   :cl-heap
   vector-name vector
   :make-fn (lambda () (make-instance 'cl-heap:priority-queue))
   :push-fn (lambda (q i) (cl-heap:enqueue q i i))
   :peek-fn (lambda (q) (cl-heap:peep-at-queue q))
   :pop-fn (lambda (q) (cl-heap:dequeue q))))

(defun test-heap (vector-name vector)
  (declare (optimize speed))
  (perform-test
   :heap
   vector-name vector
   ;; No way to pass the starting capacity.
   :make-fn (lambda () (heap:make-heap #'<))
   :push-fn (lambda (q i) (heap:heap-push i q))
   :peek-fn (lambda (q) (heap:heap-peek q))
   :pop-fn (lambda (q) (heap:heap-pop q))))

(defun test-minheap (vector-name vector)
  (declare (optimize speed))
  (perform-test
   :minheap
   vector-name vector
   ;; No way to pass the starting capacity.
   :make-fn (lambda () (make-instance 'binary-heap:binary-heap))
   :push-fn (lambda (q i) (binary-heap:insert q i i))
   :peek-fn (lambda (q) (binary-heap:peek-min q))
   :pop-fn (lambda (q) (binary-heap:extract-min q))))

(defun test-damn-fast-priority-queue (vector-name vector)
  (declare (optimize speed))
  (perform-test
   :damn-fast-priority-queue
   vector-name vector
   :make-fn (lambda () (if +pass-capacity-p+
                           (damn-fast-priority-queue:make-queue +capacity+)
                           (damn-fast-priority-queue:make-queue)))
   :push-fn (lambda (q i) (damn-fast-priority-queue:enqueue q i i))
   :peek-fn (lambda (q) (damn-fast-priority-queue:peek q))
   :pop-fn (lambda (q) (damn-fast-priority-queue:dequeue q))))

(defun test-damn-fast-stable-priority-queue (vector-name vector)
  (declare (optimize speed))
  (perform-test
   :damn-fast-stable-priority-queue
   vector-name vector
   :make-fn (lambda ()
              (if +pass-capacity-p+
                  (damn-fast-stable-priority-queue:make-queue +capacity+)
                  (damn-fast-stable-priority-queue:make-queue)))
   :push-fn (lambda (q i) (damn-fast-stable-priority-queue:enqueue q i i))
   :peek-fn (lambda (q) (damn-fast-stable-priority-queue:peek q))
   :pop-fn (lambda (q) (damn-fast-stable-priority-queue:dequeue q))))

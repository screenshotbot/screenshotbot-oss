;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-hash-lock
  (:use #:cl
        #:util/hash-lock
        #:fiveam)
  (:import-from #:util/hash-lock
                #:%hash-table)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-hash-lock)

(util/fiveam:def-suite)

(defmacro with-mp (&body body)
  #-buck
  `(progn ,@body)
  #+buck
  `(mp:initialize-multiprocessing
    "test-thread"
    nil
    (lambda ()
      ,@body)))

(test happy-path
  (let ((hash-lock (make-instance 'hash-lock)))
    (let (ret)
      (with-hash-lock-held (2 hash-lock)
        (setf ret t))
      (is (eql t ret))))
  (pass))

(test stress-test
  (with-mp
   (let* ((times 1000)
          (threads 8)
          (lock (bt:make-lock))
          (hash-lock (make-instance 'hash-lock))
          (ref 0)
          (threads (loop for x below threads
                         collect
                         (bt:make-thread
                          (lambda ()
                            (loop for i below times
                                  do
                                     (with-hash-lock-held ('foo hash-lock)
                                       (setf ref (+ 10 ref)))))))))
     (loop for th in threads
           do (bt:join-thread th))
     (is (eql 0 (hash-table-count (%hash-table hash-lock))))
     (is (eql 80000 ref)))))

(defvar *dummy* "berg")

(test stress-test-with-equal
  (with-mp
   (let* ((times 100)
          (threads 3)
          (hash-lock (make-instance 'hash-lock :test #'equal))
          (ref 0)
          (threads (loop for x below threads
                         collect
                         (bt:make-thread
                          (lambda ()
                            (sleep 0.1)
                            (loop for i below times
                                  do
                                     (with-hash-lock-held ((format nil "zoid~a" *dummy*) hash-lock)
                                       (setf ref (+ 10 ref)))))))))
     (loop for th in threads
           do (bt:join-thread th))
     (is (eql 3000 ref)))))

(define-condition test-error (error)
  ())

(test unwind
  (let ((hash-lock (make-instance 'hash-lock)))
    (unwind-protect
         (signals test-error
          (with-hash-lock-held (t hash-lock)
            (error 'test-error)))
      (is (eql 0 (hash-table-count (%hash-table hash-lock)))))))

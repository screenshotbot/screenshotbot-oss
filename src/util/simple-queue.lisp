;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/simple-queue
  (:use #:cl)
  (:export
   #:make-queue
   #:dequeue
   #:enqueue
   #:queue-emptyp
   #:queue-length
   #:queue-items))
(in-package :util/simple-queue)

(defvar *lock* (bt:make-lock)
  "Only used for enqueue-with-max-length")

(defclass queue ()
  ((head :accessor head
         :initform nil)
   (tail :accessor tail
         :initform nil)
   (length :initform 0
           :reader queue-length
           :accessor %queue-length)))

(defun make-queue ()
  (make-instance 'queue))

(defmethod enqueue (value (queue queue))
  (let ((new-tail (cons value nil)))
    (incf (%queue-length queue))
    (cond
      ((not (head queue))
       (setf (head queue) new-tail)
       (setf (tail queue) new-tail))
      (t
       (setf (cdr (tail queue)) new-tail)
       (setf (tail queue) new-tail)))))

(defmethod enqueue-with-max-length (value (queue queue) &key max-length)
  "Enqueues a new element, and keeps dequeing until the max-length is
reached.

Thread safe is enqueue-with-max-length is the only way you're
accessing the queue.

Convenient for keeping in-memory logs."
  (bt:with-lock-held (*lock*)
    (enqueue value queue)
    (loop while (> (queue-length queue) max-length)
          do
             (dequeue queue))))

(defun dequeue (queue)
  (let ((head (head queue)))
    (setf (head queue) (cdr head))
    (unless (head queue)
      (decf (%queue-length queue))
      (setf (tail queue) nil))
    (car head)))

(defun queue-emptyp (queue)
  (null (head queue)))

(defun queue-items (queue)
  (copy-seq (head queue)))

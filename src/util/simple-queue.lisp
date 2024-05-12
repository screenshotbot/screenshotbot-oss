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

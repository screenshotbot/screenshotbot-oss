;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/hash-lock
  (:use #:cl)
  (:import-from #:util/macros
                #:def-easy-macro)
  (:import-from #:lparallel.promise
                #:fulfill)
  (:import-from #:lparallel.promise
                #:promise)
  (:import-from #:lparallel.promise
                #:fulfill)
  (:import-from #:lparallel.promise
                #:chain)
  (:import-from #:lparallel.promise
                #:future)
  (:export
   #:hash-lock
   #:with-hash-lock-held
   #:hash-locked-future)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/hash-lock)

(defclass object-state ()
  ((item-lock :initform (bt:make-lock)
              :reader item-lock)
   (future-queue :accessor future-queue
                 :initform nil)
   (future-running-p :initform nil)
   (count :initform 0
          :accessor %count
          :documentation "Total number of threads that are waiting or running. This does not
 include future-queue, but will include any future that is currently
 running.")))

(defclass hash-lock ()
  ((lock :initform (bt:make-lock "hash-lock")
         :reader %lock)
   (hash-table :accessor %hash-table)))

(defmethod initialize-instance :after ((self hash-lock) &key (test #'eql) &allow-other-keys)
  (setf (%hash-table self)
        (make-hash-table :test test)))

(defmacro with-hash-lock-held ((obj hash-lock) &body body)
  `(call-with-hash-lock-held
    ,obj
    ,hash-lock
    (lambda () ,@body)))

(defun object-state (obj hash-lock)
  "Get the object state for the given object. Must be called under the
  lock!"
  (symbol-macrolet ((x (gethash obj (%hash-table hash-lock))))
    (or x
        (setf x (make-instance 'object-state)))))

(defun call-with-hash-lock-held (obj hash-lock fn)
  (let (object-state)
    (bt:with-lock-held ((%lock hash-lock))
      (setf object-state (object-state obj hash-lock))
      ;; number of threads waiting on this
      (incf (%count object-state)))
    (unwind-protect
         (bt:with-lock-held ((item-lock object-state))
           (funcall fn))
      (bt:with-lock-held ((%lock hash-lock))
        (decf (%count object-state))
        (cond
          ((<= (%count object-state) 0)
           ;; we were the last one waiting on this, we can free up all
           ;; the resource associated with this object
           (remhash obj (%hash-table hash-lock))))))))

(defclass hash-locked-future ()
  ((promise :initarg :promise
            :reader hash-locked-promise)
   (body :initarg :body
         :reader body)
   (kernel :initarg :kernel
           :reader kernel)))

(def-easy-macro hash-locked-future (obj hash-lock &fn body)
  "Like future, but we ensure the lock is held before running the future.
 We can be a little more efficient by ensuring that only one object is
 running at any point of time."
  (let ((promise (promise)))
    (bt:with-lock-held ((%lock hash-lock))
      (let ((obj-state (object-state obj hash-lock)))
        (a:appendf
         (future-queue obj-state)
         (list
          (make-instance 'hash-locked-future
                         :promise promise
                         :body body
                         :kernel lparallel:*kernel*)))
        (unless (second (future-queue obj-state))
          ;; If there's exactly one item on the list, then we should
          ;; trigger the next future.
          (trigger-next-future obj-state hash-lock))))
    promise))

(defun trigger-next-future (object-state hash-lock)
  "Trigger the next future in the queue. Make sure you're holding the
 hash-lock's lock before calling this function"
  (let ((next-future (car (future-queue object-state))))
    (let ((lparallel:*kernel* (kernel next-future)))
      (fulfill (hash-locked-promise next-future)
        (chain
         (future
          (unwind-protect
               (bt:with-lock-held ((item-lock object-state))
                 (funcall (body next-future)))
            (bt:with-lock-held ((%lock hash-lock))
              (pop (future-queue object-state))
              (when (future-queue object-state)
                (trigger-next-future object-state hash-lock))))))))))

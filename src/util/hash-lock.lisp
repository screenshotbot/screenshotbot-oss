;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/hash-lock
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:hash-lock
   #:with-hash-lock-held))
(in-package :util/hash-lock)

(defclass object-state ()
  ((cv :initform (bt:make-condition-variable)
       :reader cv)
   (count :initform 0
          :accessor %count)))

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
      (incf (%count object-state)) ;; number of threads waiting on this
      (when (> (%count object-state) 1)
        ;; Whoever is in-charge of this object will notify us when
        ;; they are done.
        (bt:condition-wait (cv object-state) (%lock hash-lock))))
    (funcall fn)
    (bt:with-lock-held ((%lock hash-lock))
      (decf (%count object-state))
      (cond
       ((> (%count object-state) 0)
         ;; there are other threads waiting for control
        (bt:condition-notify (cv object-state)))
       (t
        ;; we were the last one waiting on this, we can free up all
        ;; the resource associated with this object
        (remhash obj (%hash-table hash-lock)))))))

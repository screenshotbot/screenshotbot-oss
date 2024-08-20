;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/benchmark
  (:nicknames :benchmark)
  (:use #:cl)
  (:import-from #:alexandria
                #:assoc-value)
  (:export #:def-benchmark
           #:measure
           #:run))
(in-package :util/benchmark)

(defun clock-ns ()
  "Get a time unit in nanoseconds. Might not be accurate all the way
upto nanoseconds though."
  (*
   (get-internal-real-time)
   (floor 1000000000 internal-time-units-per-second)))

(defparameter *min-benchmark-time* (* 4 1000 1000 1000))

(defvar *in-benchmark-p* nil)

(defmacro measure (&body body)
  (let ((iter (gensym "iter"))
        (i (gensym "i")))
    `(call-measure
      (lambda (,iter)
        (declare (type fixnum ,iter))
        (dotimes (,i ,iter)
          (declare (ignore ,i))
          ,@body)))))

(defun call-measure (fn)
  (unless *in-benchmark-p*
    (warn "Calling MEASURE outside of a def-benchmark can lead to measurement issues, since the code may not be compiled"))
  ;; warmup
  (funcall fn 1)
  (let ((start-time (clock-ns))
        (iterations 0)
        (next-count 1))
    (declare (type fixnum iterations next-count
                   start-time))
    (loop
      (funcall fn next-count)
      (incf iterations next-count)
      (setf next-count (* 2 next-count))
      (let ((time-spent (- (clock-ns) start-time)))
        (when (> time-spent *min-benchmark-time*)
          (return (ceiling time-spent iterations)))))))

(defvar *benchmarks* nil)

(defclass benchmark ()
  ((name :initarg :name
         :reader benchmark-name)
   (impl :initarg :impl
         :reader benchmark-impl)))

(defmacro def-benchmark (name &body body)
  `(setf
    (assoc-value *benchmarks* ',name)
    (make-instance 'benchmark
                   :name ',name
                   :impl (lambda ()
                           ,@body))))

(defmethod run ((self benchmark))
  (let ((*in-benchmark-p* t))
    (funcall (benchmark-impl self))))

(defmethod run ((name symbol))
  (let ((benchmark (assoc-value *benchmarks* name)))
    (unless benchmark
      (error "could not find benchmark named: ~a" name))
    (run
     benchmark)))

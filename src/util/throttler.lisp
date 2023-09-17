;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/throttler
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:local-time
                #:timestamp-difference)
  (:export
   #:keyed-throttler))
(in-package :util/throttler)

(defclass throttler ()
  ((max-tokens :initarg :tokens
               :reader max-tokens
               :documentation "The max tokens per period")
   (period :initarg :period
           :reader period
           :initform 3600
           :documentation "interval in seconds")

   ;; Internal
   (tokens :initarg :tokens
           :accessor %tokens
           :documentation "internal slot, for current number of tokens.")
   (last-updated :initform (get-universal-time)
                 :initarg :now
                 :accessor %last-updated)
   (lock :initform (bt:make-lock)
         :reader lock))
  (:documentation "A throttler using the Token Bucket algorithm. You specify the number
of requests allowed (or tokens) per certain period. For instance you
might say 1000 requests per hour.

If the request is throttled, then an error is signaled."))

(define-condition throttled-error (error)
  ())

(defclass keyed-throttler ()
  ((max-tokens :initarg :tokens
               :initform (error "must provide :tokens")
               :reader max-tokens)
   (period :initarg :period
           :reader period
           :initform 3600
           :documentation "interval in seconds, defaults to an hour")
   (throttler-map :accessor throttler-map
                  :initform (fset:empty-map))))

(defmethod throttled-funcall ((self throttler) fn &key (now (get-universal-time))
                                                    key)
  (declare (ignore key))
  (bt:with-lock-held ((lock self))
    (incf (%tokens self)
          (* (- now (%last-updated self)) (/ (max-tokens self) (period self))))
    (setf (%last-updated self) now)
    (when (< (%tokens self) 1)
      (error 'throttled-error))
    (when (> (%tokens self) (max-tokens self))
      (setf (%tokens self) (max-tokens self)))
    (decf (%tokens self)))
  (funcall fn))

(defmethod throttler-for-key ((self keyed-throttler) key &key now)
  (with-slots (throttler-map) self
    (let ((existing (fset:@ throttler-map key)))
      (cond
        (existing
         existing)
        (t
         (atomics:atomic-update
          throttler-map
          (lambda (old)
           (fset:with
            old key (make-instance 'throttler
                                   :tokens (max-tokens self)
                                   :period (period self)
                                   :now now))))
         (fset:@ throttler-map key))))))

(defmethod throttled-funcall ((self keyed-throttler) fn &key (now (get-universal-time))
                                                          key)
  (unless key
    (error "Must provide :key for a keyed-throttler"))
  (let ((throttler (throttler-for-key self key :now now)))
    (throttled-funcall throttler fn :now now)))

(def-easy-macro with-throttling (throttler &key key &fn fn)
  (throttled-funcall throttler #'fn :key key))

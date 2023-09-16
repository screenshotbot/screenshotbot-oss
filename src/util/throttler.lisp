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
                #:timestamp-difference))
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

(defmethod throttled-funcall ((self throttler) fn &key (now (get-universal-time)))
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

(def-easy-macro with-throttling (throttler &fn fn)
  (throttled-funcall throttler #'fn))

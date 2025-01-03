;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/thresholds/dsl
  (:use #:cl)
  (:export #:exact-images))
(in-package :screenshotbot/thresholds/dsl)


(defun %read-dsl-from-string (code)
  (let ((input (make-string-input-stream code)))
    (let ((*package* (symbol-package 'foo)))
      (%read-dsl input))))

(defun read-list (stream &optional read-so-far)
  (let ((next (peek-char nil stream)))
    (cond
      ((eql #\) next)
       (read-char stream)
       (reverse read-so-far))
      (t
       (read-list
        stream
        (list* (%read-dsl stream) read-so-far))))))

(defun read-symbol-as-string (stream)
  (coerce
   (loop for next = (peek-char nil stream)
         until (or
                (eql next #\)))
         collect (read-char stream))
   'string))

(defparameter *symbols*
  '(exact-images))

(defun parse-symbol (string)
  (loop for sym in *symbols*
        if (equal (string-downcase sym) string)
          return sym
        finally
         (error "Invalid symbol: ~a" string)))

(defun %read-dsl (stream)
  (let ((next (peek-char nil stream)))
    (cond
      ((eql #\( next)
       (read-char stream)
       (read-list stream))
      (t
       (parse-symbol
        (read-symbol-as-string stream))))))




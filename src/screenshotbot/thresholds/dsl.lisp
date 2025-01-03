;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/thresholds/dsl
  (:use #:cl)
  #+lispworks
  (:import-from #:lw
                #:whitespace-char-p)
  (:export #:exact-images))
(in-package :screenshotbot/thresholds/dsl)

#-lispworks
(defun whitespace-char-p (ch)
  (ppcre:scan-to-strings "\\s" (string ch)))

(defun %read-dsl-from-string (code)
  (let ((input (make-string-input-stream code)))
    (discard-whitespace input)
    (let ((*package* (symbol-package 'foo)))
      (%read-dsl input))))

(defun read-list (stream &optional read-so-far)
  (let ((next (peek-char nil stream)))
    (cond
      ((eql #\) next)
       (read-char stream)
       (discard-whitespace stream)
       (reverse read-so-far))
      (t
       (read-list
        stream
        (list* (%read-dsl stream) read-so-far))))))

(defun read-symbol-as-string (stream)
  (prog1
      (coerce
       (loop for next = (peek-char nil stream)
             until (or
                    (eql next #\))
                    (whitespace-char-p next))
             collect (read-char stream))
       'string)
    (discard-whitespace stream)))

(defparameter *symbols*
  '(exact-images
    + * / -))


(defun discard-whitespace (stream)
  (loop for ch = (peek-char nil stream nil nil)
        while (and ch (whitespace-char-p ch))
        do (read-char stream)))

(defun parse-symbol (string)
  (cond
    ((ppcre:scan "\\d+" string)
     (parse-integer string))
    (t
     (loop for sym in *symbols*
           if (equal (string-downcase sym) string)
             return sym
           finally
              (error "Invalid symbol: ~a" string)))))

(defun %read-dsl (stream)
  (let ((next (peek-char nil stream)))
    (cond
      ((eql #\( next)
       (read-char stream)
       (discard-whitespace stream)
       (read-list stream))
      (t
       (parse-symbol
        (read-symbol-as-string stream))))))

(defun eval-dsl (dsl)
  "Takes a DSL and returns a function that can be evaluated later."
  (eval `(lambda () ,dsl)))





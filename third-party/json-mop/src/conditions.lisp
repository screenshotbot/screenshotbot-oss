;; Copyright (c) 2015 Grim Schjetne
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(in-package #:json-mop)

(define-condition slot-not-serializable (warning)
  ((slot-name :initarg :slot-name
              :reader slot-name))
  (:report (lambda (condition stream)
             (format stream "Slot ~A has no JSON metadata associated with it."
                     (slot-name condition)))))

;; TODO: inherit TYPE-ERROR and change all occurences to specify
;; :expected-type, likewise change relevant occurrences of TYPE-ERROR
;; to JSON-TYPE-ERROR
(define-condition json-type-error (error)
  ((json-type :initarg :json-type
              :reader json-type)))

(define-condition null-value (json-type-error) ())

(define-condition null-in-homogeneous-sequence (json-type-error) ()
  (:report (lambda (condition stream)
             (format stream "null encountered in a homogeneous sequence of type ~S"
                     (json-type condition)))))

(define-condition no-values-parsed (warning)
  ((hash-table :initarg :hash-table
               :reader no-values-hash-table)
   (class-name :initarg :class-name
               :reader no-values-class))
  (:report (lambda (condition stream)
             (format stream "No keys corresponding to slots in ~A found in ~A"
                     (no-values-class condition)
                     (no-values-hash-table condition)))))

(defun read-eval-query ()
  (format *query-io* "Eval: ")
  (list (eval (read *query-io*))))

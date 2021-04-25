;;;
;;;
;;; Copyright (c) 2009, Jean-Claude Beaudoin
;;; All rights reserved by the author.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;;
;;;

(in-package :jni)


;(declaim (optimize debug))
;(declaim (optimize (debug 0) (speed 3)))


(defvar *lisp-external-references* 
  (let ((array (make-array '(1000) :adjustable t :fill-pointer 1)))
    (setf (aref array 0) nil)  ;; we reserve index 0 for special use.
    array
    )
  )

(defvar *lisp-ext-ref-free-head* nil) ;; should be an integer, index of the first
;; free cell in the allocated portion of *lisp-external-references*. If nil it means
;; that the free cell chain is empty.

(defvar +lisp-ext-ref-lock+ (bt:make-lock "jni:*lisp-external-references*"))


(defun new-lisp-reference (obj)
  (bt:with-lock-held (+lisp-ext-ref-lock+)
    (cond ((eq nil obj)
	   0
	   )
	  (*lisp-ext-ref-free-head*
	   (let* ((head *lisp-ext-ref-free-head*)
		  (next (aref *lisp-external-references* head)))
	     (setf (aref *lisp-external-references* head) obj)
	     (setq *lisp-ext-ref-free-head* next)
	     head
	     )
	   )
	  (t
	   (vector-push-extend obj *lisp-external-references*)
	   )
	  )
    )
  )

(defun free-lisp-reference (index)
  (unless (eql 0 index)
    (bt:with-lock-held (+lisp-ext-ref-lock+)
      (setf (aref *lisp-external-references* index) *lisp-ext-ref-free-head*)
      (setq *lisp-ext-ref-free-head* index)
      )
    )
  )

(defun lisp-reference-value (obj)
  (aref *lisp-external-references* obj)
  )

(defun lisp-reference-eq (ref1 ref2)
  (eq (lisp-reference-value ref1) (lisp-reference-value ref2))
  )

#|

(export '(new-lisp-reference
	  free-lisp-reference
	  lisp-reference-value
	  lisp-reference-eq))

|#


(defmacro new-lref (obj) `(new-lisp-reference ,obj))
(defmacro free-lref (index) `(free-lisp-reference ,index))
(defmacro lref-val (obj) `(lisp-reference-value ,obj))
(defmacro lref-eq (ref1 ref2) `(lisp-reference-eq ,ref1 ,ref2))

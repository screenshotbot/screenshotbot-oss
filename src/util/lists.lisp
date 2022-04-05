(defpackage #:util/lists
  (:use #:cl)
  (:export #:head
	   #:tail))
(in-package #:util/lists)

;; See also 'last' and 'butlast'

(defun fix-n (list n)
  (if (< n 0)
      (+ (length list) n)
      n))

(defun head (list n)
  "Returns the first n elements of the list, or the entire list if the list has fewer than n elements"
  (let ((n (fix-n list n)))
    (labels ((head (list n prefix)
               (cond
                 ((eq list nil) (nreverse prefix))
                 ((eq n 0) (nreverse prefix))
                 (t
                  (head (cdr list) (-  n 1)
                        (list* (car list) prefix))))))
      (head list n nil))))

(defun tail (list n)
  "Returns all the elements from the (n+1)th element"
  (let ((n (fix-n list n)))
    (cond
      ((eq list nil) nil)
      ((<= n 0) list)
      (t (tail (cdr list) (- n 1))))))

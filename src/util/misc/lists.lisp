(defpackage #:util/misc/lists
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export #:head
	   #:tail
       #:make-batches
       #:with-batches))
(in-package #:util/misc/lists)

;; See also 'last' and 'butlast'

(defun fix-n (list n)
  (if (< n 0)
      (+ (length list) n)
      n))

(defun head (list n &key (filter (lambda (x) (declare (ignore x)) t)))
  "Returns the first n elements of the list, or the entire list if the list has fewer than n elements.

 If filter is provided, it returns only the first n elements that matches the filter.

In either case, we also return a second value which the all the
remaining elements (unfiltered)"
  (let ((n (fix-n list n)))
    (labels ((head (list n prefix)
               (cond
                 ((or
                   (eql list nil)
                   (eql n 0))
                  (values (nreverse prefix) list))
                 ((funcall filter (car list))
                  (head (cdr list) (-  n 1)
                        (list* (car list) prefix)))
                 (t
                  (head (cdr list) n prefix)))))

      (head list n nil))))

(defun tail (list n)
  "Returns all the elements from the (n+1)th element"
  (let ((n (fix-n list n)))
    (cond
      ((eq list nil) nil)
      ((<= n 0) list)
      (t (tail (cdr list) (- n 1))))))

(def-easy-macro with-batches (&binding batch
                                       list &fn fn &key (batch-size 10)
                                       &binding index)

  (labels ((wrapped-fn (batch ctr)
             (restart-case
                 (funcall fn batch ctr)
               (restart-batch ()
                 (wrapped-fn batch ctr))))
           (call-next (list ctr)
             (multiple-value-bind (batch rest)
                 (util/misc/lists:head list batch-size)
               (when batch
                 (wrapped-fn batch ctr)
                 (call-next rest (+ ctr (length batch)))))))
    (call-next list 0)))

(defun make-batches (list &key (batch-size 10))
  (let (res
        indexes)
    (with-batches (batch list :batch-size batch-size
                   :index index)
      (push batch res)
      (push index indexes))
    (values
     (reverse res)
     (reverse indexes))))

(pkg:define-package :fiveam-matchers/lists
    (:use #:cl
          #:alexandria)
  (:import-from #:fiveam-matchers/core
                #:self-describing-list
                #:describe-self
                #:esc
                #:describe-mismatch
                #:matchesp
                #:matcher)
  (:export
   #:contains))

(defclass contains (matcher)
  ((expected :initarg :expected
             :accessor expected)))

(defun contains (&rest expected)
  (make-instance 'contains
                 :expected expected))

(defmethod matchesp ((matcher contains) (actual list))
  (when (eql (length actual)
             (length (expected matcher)))
    (not
     (loop for a in actual
           for x in (expected matcher)
           unless (matchesp x a)
             return t))))

(defmethod describe-self ((matcher contains))
  `("a list with values: "
    ,(self-describing-list
      (expected matcher))))

(defmethod describe-mismatch ((matcher contains) (actual list))
  (cond
    ((not (eql (length actual)
               (length (expected matcher))))
     `("was a list not of length " ,(length (expected matcher))))
    (t
     (loop for a in actual
           for x in (expected matcher)
           for i from 0
           unless (matchesp x a)
             return `("The element at position "
                      ,(esc i)
                      " "
                      ,(describe-mismatch x a))))))

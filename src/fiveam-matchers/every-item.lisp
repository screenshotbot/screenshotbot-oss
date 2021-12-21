(defpackage :fiveam-matchers/every-item
  (:use #:cl
        #:fiveam-matchers/core)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:every-item))
(in-package :fiveam-matchers/every-item)

(defclass every-item (matcher)
  ((expected :initarg :expected
             :accessor expected)))

(defun every-item (expected)
  (make-instance 'every-item :expected (ensure-matcher  expected)))

(defmethod matchesp ((self every-item) actual)
  (loop for item in actual
        always (matchesp (expected self) item)))

(defmethod describe-self ((self every-item))
  `("A sequence where every item matches: " ,(describe-self (expected self))))

(defmethod describe-mismatch ((self every-item) actual)
  (loop for item in actual
        for i from 0
        if (not (matchesp (expected self) item))
          return `("The element at "
                   ,i " mismatched with: "
                   ,(describe-mismatch (expected self) item))
        finally
        (return "Could not describe mismatch, possible bug")))

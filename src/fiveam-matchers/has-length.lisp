(defpackage :fiveam-matchers/has-length
  (:use #:cl
        #:fiveam-matchers/core)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:has-length))
(in-package :fiveam-matchers/has-length)

(defclass has-length (matcher)
  ((expected :initarg :expected
             :accessor expected)))

(defun has-length (expected)
  (make-instance 'has-length :expected expected))

(defmethod matchesp ((self has-length) actual)
  (= (expected self) (length actual)))

(defmethod describe-self ((self has-length))
  `("a sequence that has length " ,(expected self )))

(defmethod describe-mismatch ((self has-length) actual)
  `("got a "
    ,(ecase (type-of actual)
       (null
        'list)
       (cons
        'list)
       (t (type-of actual)))
    " of length " ,(length actual)))

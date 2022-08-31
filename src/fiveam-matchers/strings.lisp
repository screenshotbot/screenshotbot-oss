(defpackage :fiveam-matchers/strings
  (:use #:cl
        #:fiveam-matchers/core)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:starts-with))
(in-package :fiveam-matchers/strings)

(defclass starts-with-matcher (matcher)
  ((prefix :initarg :prefix
           :reader prefix)))

(defmethod starts-with ((prefix string))
  (make-instance 'starts-with-matcher
                  :prefix prefix))

(defmethod matchesp ((matcher starts-with-matcher)
                     actual)
  (and
   (stringp actual)
   (str:starts-with-p (prefix matcher)
                      actual)))

(defmethod describe-self ((matcher starts-with-matcher))
  `("a string that starts with `" ,(prefix matcher) "`"))

(defmethod describe-mismatch ((matcher starts-with-matcher) actual)
  `("expected `" ,actual "` to start with " ,(prefix matcher) ))

(defpackage :util/tests/test-single
  (:use #:cl
        #:fiveam)
  (:import-from #:util/single
                #:deserialize
                #:serialize)
  (:import-from #:fiveam-matchers/core
                #:equal-to
                #:has-typep
                #:assert-that)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-single)


(util/fiveam:def-suite)

(defclass dummy-1 ()
  ((foo :initarg :foo)
   (bar :initarg :bar)))

(def-fixture state ()
  (uiop:with-temporary-file (:stream s :direction :io :element-type '(unsigned-byte 8))
    (&body)))

(test preconditions ()
  (with-fixture state ()
    (let ((obj (make-instance 'dummy-1 :foo "bar")))
      (finishes
        (serialize obj s))
      (file-position s 0)
      (let ((result (deserialize s)))
        (assert-that result
                     (has-typep 'dummy-1))
        (assert-that (slot-value result 'foo)
                     (equal-to "bar"))
        (is-false (slot-boundp result 'bar))))))

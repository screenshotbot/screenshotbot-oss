(uiop:define-package :util/tests/test-make-instance-with-accessors
  (:use #:cl
        #:fiveam
        #:alexandria
        #:util/make-instance-with-accessors))
(in-package :util/tests/test-make-instance-with-accessors)

(util/fiveam:def-suite)

(defclass test-class ()
  ((foo :initarg :foo
        :accessor test-class-foo)
   (car :initarg :carrot
        :accessor test-class-carrot)))

(test preconditions
  (let ((obj (make-instance-with-accessors 'test-class
                                           'test-class-foo 22
                                           'test-class-carrot "bleh")))
    (is (eql 22 (test-class-foo obj)))
    (is (equal "bleh" (test-class-carrot obj)))))

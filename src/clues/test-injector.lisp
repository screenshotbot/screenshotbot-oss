(defpackage :clues/test-injector
  (:use #:cl
        #:fiveam)
  (:import-from #:clues/injector
                #:injector
                #:get-instance)
  (:local-nicknames (#:a #:alexandria)))
(in-package :clues/test-injector)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((injector (make-instance 'injector)))
    (&body)))

(defclass simple-class ()
  ())

(test simple-class
  (with-fixture state ()
   (is (eql 'simple-class
            (type-of (get-instance injector (find-class 'simple-class)))))))

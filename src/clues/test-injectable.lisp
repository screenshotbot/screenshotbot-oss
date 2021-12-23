(defpackage :clues/test-injectable
  (:use #:cl
        #:fiveam)
  (:import-from #:clues/injectable
                #:%injector
                #:injectable)
  (:import-from #:clues/injector
                #:injector
                #:singleton
                #:scope
                #:make-instance
                #:get-instance)
  (:import-from #:closer-mop
                #:slot-definition-name
                #:class-slots)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:has-item
                #:contains)
  (:local-nicknames (#:a #:alexandria)))
(in-package :clues/test-injectable)

(util/fiveam:def-suite)

(defclass dep ()
  ()
  (:metaclass injectable))

(defclass test-class ()
  ((un-injected-slot :initform nil)
   (dummy-val :initform 2
              :reader dummy-val)
   (dep :inject dep
        :reader dep))
  (:metaclass injectable))


(test make-injectable
  (finishes
    (make-instance 'test-class))
  (let ((injector (make-instance 'injector)))
    (let ((inst (get-instance injector 'test-class)))
      (is-true inst)
      (is-true (dep inst))
      (is (typep (dep inst) 'dep)))))

(defclass my-singleton ()
  ()
  (:metaclass injectable)
  (:scope singleton))


(test singleton
  (let ((injector (make-instance 'injector)))
    (let ((inst-1 (get-instance injector 'my-singleton))
          (inst-2 (get-instance injector 'my-singleton)))
      (is (eql inst-1 inst-2)))))

(test slots
  (let ((slots (mapcar 'slot-definition-name (class-slots (find-class 'test-class)))))
   (assert-that
    slots
    (has-item 'dep))
   (assert-that
    slots
    (has-item 'dummy-val))))

(test accessor
  (is (eql 2 (slot-value (make-instance 'test-class) 'dummy-val)))
  (is (eql 2 (dummy-val (make-instance 'test-class)))))

(test with-injector-slots
  (defclass my-test-class ()
    (foo)
    (:metaclass injectable))
  (closer-mop:finalize-inheritance (find-class 'my-test-class))
  (is-true (class-slots (find-class 'my-test-class)))
  (assert-that
   (mapcar 'slot-definition-name
           (class-slots (find-class 'my-test-class)))
   (has-item '%injector))
  (pass))

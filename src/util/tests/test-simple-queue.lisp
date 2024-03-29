(defpackage :util/tests/test-simple-queue
  (:use #:cl
        #:fiveam)
  (:import-from #:util/simple-queue
                #:queue-emptyp
                #:tail
                #:head
                #:dequeue
                #:enqueue
                #:make-queue))
(in-package :util/tests/test-simple-queue)

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((q (make-queue)))
    (&body)))

(test preconditions
  (with-fixture state ()
    (pass)))

(test enqueue-dequeue
  (with-fixture state ()
    (enqueue q 1)
    (is (eql 1 (dequeue q)))))

(test removes-from-queue-though
  (with-fixture state ()
    (enqueue q 1)
    (is (eql 1 (dequeue q)))
    (is (eql nil (dequeue q)))))

(test insert-two-and-delete-two
  (with-fixture state ()
    (enqueue q 1)
    (enqueue q 2)
    (is (eql 1 (dequeue q)))
    (is (eql 2 (dequeue q)))
    (is (eql nil (dequeue q)))))

(test clear-up-the-tail-too
  (with-fixture state ()
    (enqueue q 1)
    (is (eql 1 (dequeue q)))
    (enqueue q 2)
    (is (eql 2 (dequeue q)))
    (is (eql nil (dequeue q)))))

(test internal-consistency-after-removing-everythin
  "This doesn't affect the correctness, but is useful to avoid any future bugs"
  (with-fixture state ()
    (enqueue q 1)
    (is (eql 1 (dequeue q)))
    (is (eql nil (head q)))
    (is (eql nil (tail q)))))

(test emptyp
  (with-fixture state ()
    (is-true (queue-emptyp q))
    (enqueue q 1)
    (is-false (queue-emptyp q))
    (dequeue q)
    (is-true (queue-emptyp q))))

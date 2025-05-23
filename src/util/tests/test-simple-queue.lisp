(defpackage :util/tests/test-simple-queue
  (:use #:cl
        #:fiveam)
  (:import-from #:util/simple-queue
                #:enqueue-with-max-length
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
    (enqueue 1 q)
    (is (eql 1 (dequeue q)))))

(test removes-from-queue-though
  (with-fixture state ()
    (enqueue 1 q)
    (is (eql 1 (dequeue q)))
    (is (eql nil (dequeue q)))))

(test insert-two-and-delete-two
  (with-fixture state ()
    (enqueue 1 q)
    (enqueue 2 q)
    (is (eql 1 (dequeue q)))
    (is (eql 2 (dequeue q)))
    (is (eql nil (dequeue q)))))

(test clear-up-the-tail-too
  (with-fixture state ()
    (enqueue 1 q)
    (is (eql 1 (dequeue q)))
    (enqueue 2 q)
    (is (eql 2 (dequeue q)))
    (is (eql nil (dequeue q)))))

(test internal-consistency-after-removing-everythin
  "This doesn't affect the correctness, but is useful to avoid any future bugs"
  (with-fixture state ()
    (enqueue 1 q)
    (is (eql 1 (dequeue q)))
    (is (eql nil (head q)))
    (is (eql nil (tail q)))))

(test emptyp
  (with-fixture state ()
    (is-true (queue-emptyp q))
    (enqueue 1 q)
    (is-false (queue-emptyp q))
    (dequeue q)
    (is-true (queue-emptyp q))))

(test enqueue-with-max-length
  (with-fixture state ()
    (enqueue 1 q)
    (enqueue 2 q)
    (enqueue 3 q)
    (enqueue 4 q)
    (enqueue-with-max-length 5 q :max-length 2)))

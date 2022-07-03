(defpackage :screenshotbot/test-analytics
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/analytics
                #:map-analytics-events
                #:push-analytics-event
                #:event-session
                #:analytics-event
                #:%write-analytics-events
                #:write-analytics-events
                #:all-analytics-events
                #:*events*)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:util/store
                #:with-test-store)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-analytics)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (let ((*events* nil)
         (ev1 (make-instance 'analytics-event
                              :session "foo")))
     (&body))))

(test preconditions
  (with-fixture state ()
   (is (eql nil (all-analytics-events)))))

(test read-write-analytics
  (with-fixture state ()
    (push ev1 *events*)
    (is (equal (list ev1)  (all-analytics-events)))
    (%write-analytics-events)
    (is (eql nil *events*))

    ;; the restored object is not EQL!
    (is (not (equal (list ev1)  (all-analytics-events))))

    ;; but it should be properly restored
    (is (equal "foo" (event-session (car (all-analytics-events)))))))

(test push-analytics-event
  (with-fixture state ()
    (with-fake-request ()
      (auth:with-sessions ()
        (push-analytics-event)
        (is (eql 1 (length *events*)))
        (write-analytics-events)
        (is (eql 1 (length (all-analytics-events))))))))

(test push-a-lot-of-analytics-events
  (with-fixture state ()
    (with-fake-request ()
      (auth:with-sessions ()
        (loop for i below 100
              do (push-analytics-event))
        (is (eql 100 (length *events*)))))))

(test map-analytics-event
  (with-fixture state ()
    (push ev1 *events*)
    (is (equal (list "foo")
               (map-analytics-events #'event-session)))
    (is (equal nil
               (map-analytics-events #'event-session
                                       :keep-if (lambda (x)
                                                  (declare (ignore x))
                                                  nil))))
    (dotimes (i 100)
      (push ev1 *events*))
    (is (equal (list "foo" "foo" "foo")
               (map-analytics-events #'event-session
                                       :limit 3)))
    (write-analytics-events)
    (is (equal (list "foo" "foo" "foo")
               (map-analytics-events #'event-session
                                       :limit 3)))))

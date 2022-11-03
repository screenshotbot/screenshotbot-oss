(defpackage :screenshotbot/test-analytics
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/analytics
                #:make-digest
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
  (:import-from #:screenshotbot/events
                #:with-db
                #:*event-engine*
                #:db-engine)
  (:import-from #:fiveam-matchers/lists
                #:has-item
                #:contains)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:assert-that)
  (:import-from #:fiveam-matchers/misc
                #:is-null)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-analytics)

(util/fiveam:def-suite)

(def-fixture state ()
  (uiop:with-temporary-file (:pathname pathname)
    (with-test-store ()
      (let* ((*event-engine*
              (make-instance 'db-engine
                             :connection-spec
                             `(,(namestring pathname))
                             :database-type :sqlite3)))
        (with-db (db *event-engine*)
          (clsql:query
           "create table analytics (ip_address text, session text, script_name text,
                                    referrer text,
                                    user_agent text,
                                     query_string text, ts datetime)"
           :database db))
        (let ((*events* nil)
              (ev1 (make-instance 'analytics-event
                                  :session (make-digest "foo"))))
          (&body))))))

(test preconditions
  (with-fixture state ()
   (is (eql nil (all-analytics-events)))))

(test read-write-analytics
  (with-fixture state ()
    (push ev1 *events*)
    (assert-that (all-analytics-events)
                 (described-as "We should look up the uncached versions before we save it"
                  (contains ev1)))
    (%write-analytics-events)

    (assert-that *events*
                 (described-as "We should've reset the *events*"
                  (is-null)))

    ;; the restored object is not EQL!
    (assert-that (all-analytics-events)
                 (described-as "the objects returned are not EQL"
                   (is-not
                    (has-item ev1)))
                 (described-as "the object should still in the saved list"
                  (has-length 1)))

    ;; but it should be properly restored
    (is (equalp (make-digest "foo") (event-session (car (all-analytics-events)))))))

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
    (assert-that (map-analytics-events #'event-session)
                 (described-as "before being written"
                  (contains (make-digest "foo"))))
    (is (equalp nil
               (map-analytics-events #'event-session
                                       :keep-if (lambda (x)
                                                  (declare (ignore x))
                                                  nil))))
    (dotimes (i 100)
      (push ev1 *events*))
    (assert-that (map-analytics-events #'event-session
                                       :limit 3)
                 (described-as "We should respect the limit"
                  (contains (make-digest "foo") (make-digest "foo") (make-digest "foo"))))

    (write-analytics-events)
    (is (equalp (list (make-digest "foo") (make-digest "foo") (make-digest "foo"))
               (map-analytics-events #'event-session
                                       :limit 3)))))

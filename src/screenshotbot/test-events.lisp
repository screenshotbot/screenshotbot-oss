;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-events
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/events
                #:*events*
                #:flush-events
                #:event
                #:insert-events
                #:with-db
                #:push-event
                #:db-engine
                #:*event-engine*)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-events)

(util/fiveam:def-suite)

(def-fixture state ()
  (uiop:with-temporary-file (:pathname pathname)
   (cl-mock:with-mocks ()
     (setf *events* nil)
     (let* ((*event-engine*
              (make-instance 'db-engine
                             :connection-spec
                             `(,(namestring pathname))
                             :database-type :sqlite3)))
       (with-db (db *event-engine*)
         (clsql:query "create table event (name string, extras string, created_at datetime)"
                      :database db))
       (&body)))))

(defun row-count (db)
  (caar
   (clsql:query "select count(*) from event"
                :database db)))

(test insert-multiple-events
  (with-fixture state ()
    (with-db (db *event-engine*)
      (insert-events
       (list
        (make-instance 'event :name :foo)
        (make-instance 'event :name :bar)
        (make-instance 'event :name :car))
       db)
      (is (= 3
             (row-count db))))))

(test preconditions
  (with-fixture state ()
    (finishes
      (push-event :foobar))
    (with-db (db *event-engine*)
      (is (= 0 (row-count db))))
    (flush-events *event-engine*)
    (with-db (db *event-engine*)
      (is (= 1 (row-count db))))

    (flush-events *event-engine*)
    (with-db (db *event-engine*)
      (assert-that (row-count db)
                   (described-as "We shouln't insert again"
                    (is-equal-to 1))))))

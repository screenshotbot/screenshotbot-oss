;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/test-taskie
  (:use :cl)
  (:import-from #:it.bese.fiveam
                #:finishes
                #:is
                #:is-false
                #:test)
  (:import-from #:core/ui/taskie
                #:taskie-list
                #:taskie-row
                #:taskie-timestamp
                #:with-pagination)
  (:import-from #:util/timeago
                #:timeago))
(in-package :core/ui/test-taskie)

(util/fiveam:def-suite)

(defclass my-object ()
  ((val :initarg :val)))

(defmethod fset:compare ((a my-object)
                         (b my-object))
  (fset:compare-slots a  b 'val))

(defmethod bknr.datastore:store-object-id ((obj my-object))
  (slot-value obj 'val))

(test happy-path
  (taskie-list :empty-message "No recent runs to show. But that's okay, it's easy to get started!"
               :items (list (make-instance 'my-object :val 1)
                            (make-instance 'my-object :val 2)
                            (make-instance 'my-object :val 3))
               :next-link "/foo/next"
               :prev-link "/foo/prev"
               :row-generator (lambda (x)
                                (taskie-row
                                 :object x
                                 (taskie-timestamp :prefix "" :timestamp (local-time:now))))))

(test with-pagination-happy-path
  (let ((data (loop for i from 1 to 110 collect (make-instance 'my-object :val i))))
    (with-pagination (page data :next-link next-link :prev-link prev-link)
      (is (eql 50 (length page))))))

(test with-pagination-happy-path-with-set
  (let ((data
          (fset:convert 'fset:set
                        (loop for i from 1 to 110 collect (make-instance 'my-object :val i)))))
    (with-pagination (page data :next-link next-link :prev-link prev-link)
      (is (eql 50 (length page))))))


(test with-pagination-empty-list
  (let ((data nil))
    (with-pagination (page data :next-link next-link :prev-link prev-link)
      (is (equal nil page))
      (is-false next-link)
      (is-false prev-link))))

(test with-pagination-empty-set
  (let ((data (fset:empty-set)))
    (with-pagination (page data :next-link next-link :prev-link prev-link)
      (is (equal nil page))
      (is-false next-link)
      (is-false prev-link))))

(test timeago-happy-path
  (finishes (timeago :timestamp 40))
  (finishes (timeago :timestamp (local-time:universal-to-timestamp 40)))
  (is (equal
       (markup:write-html
        (timeago :timestamp 40))
       (markup:write-html
        (timeago :timestamp (local-time:universal-to-timestamp 40))))))

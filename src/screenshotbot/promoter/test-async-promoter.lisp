;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/promoter/test-async-promoter
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/promoter/async-promoter
                #:update-status
                #:call-on-channel-thread
                #:on-promote
                #:find-or-make-async-promoter
                #:wrapper-promoter
                #:make-sync-promoter
                #:promoters-waiting-on-commit
                #:on-commit-ready
                #:trigger-promoters-waiting-on-commit
                #:waiting-on-commit
                #:async-promoter)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:equal-to
                #:is-equal-to
                #:has-typep
                #:assert-that)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:fiveam-matchers/misc
                #:is-null
                #:is-not-null)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:screenshotbot/promote-api
                #:maybe-promote)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/promoter/test-async-promoter)

(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    (cl-mock:if-called 'call-on-channel-thread
                       (lambda (fn channel)
                         (funcall fn)))
    (with-test-store (:globally t)
      (let* ((channel (make-instance 'channel))
             (run (make-instance 'recorder-run
                                 :channel channel)))
        (&body)))))

(defclass test-async-promoter (async-promoter)
  ()
  (:metaclass persistent-class))

(defmethod on-commit-ready ((self test-async-promoter))
  "success!")

(defmethod update-status ((self test-async-promoter)))

(test simple-callback
  (with-fixture state ()
    (let ((promoter (make-instance 'test-async-promoter
                                   :run run)))
      (with-transaction ()
        (setf (waiting-on-commit promoter) "abcd"))
      (assert-that (remove-duplicates (promoters-waiting-on-commit channel "abcd"))
                   (has-length 1))
      (is (eql nil (trigger-promoters-waiting-on-commit
                    channel
                    "foo")))
      (destructuring-bind (future)
          (trigger-promoters-waiting-on-commit
           channel
           "abcd")
        (is (equal "success!"

                   (lparallel:force future)))))))


(test nil-commit-doesnt-fail
  (with-fixture state ()
    (let ((promoter (make-instance 'test-async-promoter
                                   :run run
                                   :waiting-on-commit nil)))
      (assert-that (promoters-waiting-on-commit channel "abcd")
                   (has-length 0)))))

(defclass dummy-async-promoter (async-promoter)
  ((promote-called :initform 0
                   :transient t
                   :accessor promote-called))
  (:metaclass persistent-class))

(defmethod update-status ((self dummy-async-promoter)))

(defmethod on-promote ((self dummy-async-promoter))
  (incf (promote-called self)))

(defclass dummy-async-promoter-2 (async-promoter)
  ()
  (:metaclass persistent-class))

(test wrapper
  (with-fixture state ()
    (let ((promoter (make-sync-promoter
                     'dummy-async-promoter)))
      (assert-that promoter
                   (has-typep 'wrapper-promoter)))))

(test find-or-make-async-promoter
  (with-fixture state ()
    (let ((promoter (find-or-make-async-promoter
                     'dummy-async-promoter
                     :run run)))
      (assert-that promoter
                   (has-typep 'dummy-async-promoter))
      (assert-that (find-or-make-async-promoter
                    'dummy-async-promoter
                    :run run)
                   (described-as "We should not create a new promoter"
                     (is-equal-to promoter)))
      (assert-that (find-or-make-async-promoter
                    'dummy-async-promoter-2
                    :run run)
                   (is-not-null)
                   (described-as "We should be filtering by the promoter name"
                     (is-not (equal-to promoter)))))))

(defun only-dummy-promoter ()
  (car (class-instances 'dummy-async-promoter)))

(test wrapper-callbacks
  (with-fixture state ()
    (let ((promoter (make-sync-promoter
                     'dummy-async-promoter)))
      (assert-that (only-dummy-promoter)
                   (is-null))
      (maybe-promote promoter run)

      (assert-that (only-dummy-promoter)
                   (is-not-null))

      (assert-that (promote-called (only-dummy-promoter))
                   (is-equal-to 1)))
    (let ((promoter (make-sync-promoter
                     'dummy-async-promoter)))
      (maybe-promote promoter run)
      (assert-that (promote-called (only-dummy-promoter))
                   (is-equal-to 2)))))

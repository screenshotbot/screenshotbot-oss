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
                #:assert-that)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/promoter/test-async-promoter)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store (:globally t)
    (let ((channel (make-instance 'channel)))
      (&body))))

(defclass test-async-promoter (async-promoter)
  ()
  (:metaclass persistent-class))

(defmethod on-commit-ready ((self test-async-promoter))
  "success!")

(test simple-callback
  (with-fixture state ()
    (let ((promoter (make-instance 'test-async-promoter
                                   :channel channel)))
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
                                   :channel channel
                                   :waiting-on-commit nil)))
      (assert-that (promoters-waiting-on-commit channel "abcd")
                   (has-length 0)))))

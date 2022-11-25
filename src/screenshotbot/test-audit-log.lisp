;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-audit-log
  (:use #:cl
        #:fiveam
        #:fiveam-matchers
        #:screenshotbot/audit-log)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/audit-log
                #:base-audit-log
                #:with-audit-log)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-audit-log)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(define-condition test-error (error)
  ())

(test with-audit-log-propagates-error
  (with-fixture state ()
    (signals test-error
     (with-audit-log (audit-log (make-instance 'base-audit-log))

       (error 'test-error)))
    (assert-that (class-instances 'base-audit-log)
                 (has-length 1))
    (let ((audit-log (car (class-instances 'base-audit-log))))
      (assert-that (audit-log-error audit-log)
                   (contains-string "TEST-ERROR")))
    (assert-that (with-audit-log (audit-log (make-instance 'base-audit-log))
                   :pass)
                 (equal-to :pass))))

(test if-error-is-already-set-dont-reset
  (with-fixture state ()
    (signals test-error
     (with-audit-log (audit-log (make-instance 'base-audit-log))
       (with-transaction ()
        (setf (audit-log-error audit-log) "foobar"))
       (error 'test-error)))
    (assert-that (class-instances 'base-audit-log)
                 (has-length 1))
    (let ((audit-log (car (class-instances 'base-audit-log))))
      (assert-that (audit-log-error audit-log)
                   (equal-to "foobar")))))

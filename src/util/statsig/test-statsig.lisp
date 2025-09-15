;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/statsig/test-statsig
  (:use #:cl
        #:fiveam)
  (:import-from #:util/statsig
                #:*events*)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length))
(in-package :util/statsig/test-statsig)

(util/fiveam:def-suite)

(defmethod statsig:statsig-client ((install (eql :installation)))
  (make-instance 'statsig:statsig-client))

(def-fixture state ()
  (let ((*installation* :installation))
   (unwind-protect
        (&body)
     (setf *events* nil))))

(test simple-push-event
  (with-fixture state ()
    (with-fake-request ()
      (finishes
        (statsig:push-event :hello-world))
      (assert-that
       *events*
       (has-length 1)))))

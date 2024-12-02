;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :cl-mongo-id/test-mongo-id
  (:use #:cl
        #:fiveam)
  (:import-from #:cl-mongo-id
                #:*random-value*
                #:*id-inc*
                #:reset-state))
(in-package :cl-mongo-id/test-mongo-id)

(def-suite* :cl-mongo-id)

(def-fixture state ()
  (let ((*id-inc* nil)
        (*random-value* nil))
    (&body)))

(test reset-state-happy-path
  (with-fixture state ()
    (finishes
      (reset-state))
    (is-true *id-inc*)
    (is (< *id-inc* (ash 1 24)))
    (is-true *random-value*)
    (is (= (length *random-value*) 5))))

(test happy-path-generate-oid
  (with-fixture state ()
    (reset-state)
    (is (eql 12 (length (mongoid:oid))))))

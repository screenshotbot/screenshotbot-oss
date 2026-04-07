;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-counter
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/counter
                #:defcounter
                #:next-counter)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/model/test-counter)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'company))
          (other-company (make-instance 'company)))
      (&body))))

(defcounter my-counter ())

(test simple-counter-invocation
  (with-fixture state ()
    (is (eql 1 (next-my-counter company)))
    (is (eql 2 (next-my-counter company)))
    (is (eql 3 (next-my-counter company)))))

(test each-company-get-its-own
  (with-fixture state ()
    (is (eql 1 (next-my-counter company)))
    (is (eql 1 (next-my-counter other-company)))
    (is (eql 2 (next-my-counter company)))
    (is (eql 3 (next-my-counter company)))
    (is (eql 2 (next-my-counter other-company)))))

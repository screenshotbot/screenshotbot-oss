;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-finalized-commit
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/finalized-commit
                #:finalized-commit
                #:commit-finalized-p))
(in-package :screenshotbot/model/test-finalized-commit)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'company)))
     (&body))))

(test simple-test
  (with-fixture state ()
    (is (eql nil (commit-finalized-p company "foo")))
    (make-instance 'finalized-commit
                   :company company
                   :commit "foo")
    (is (eql t (commit-finalized-p company "foo")))))

(test restricted-to-company
  (with-fixture state ()
    (let ((company-2 (make-instance 'company)))
      (make-instance 'finalized-commit
                     :company company
                     :commit "foo")
      (is (eql t (commit-finalized-p company "foo"))))))

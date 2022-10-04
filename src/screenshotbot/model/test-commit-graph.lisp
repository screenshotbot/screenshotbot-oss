;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/test-commit-graph
  (:use #:cl
        #:fiveam
        #:screenshotbot/model/commit-graph
        #:screenshotbot/model/company)
  (:import-from #:util/store
                #:with-test-store))

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (let ((company (make-instance 'company)))
     (&body))))

(test simple-find-or-create
  (with-fixture state ()
    (is (eql
         (find-or-create-commit-graph company "foo")
         (find-or-create-commit-graph company "foo")))))

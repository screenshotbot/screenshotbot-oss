;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-batch
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/batch
                #:find-or-create-batch))
(in-package :screenshotbot/model/test-batch)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'company)))
      (&body))))

(test find-instead-of-create
  (with-fixture state ()
   (is (eql
        (find-or-create-batch company "http://foo.git" "abcd")
        (find-or-create-batch company "http://foo.git" "abcd")))))

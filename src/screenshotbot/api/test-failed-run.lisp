;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/test-failed-run
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/api/failed-run
                #:%list-failed-runs
                #:parse-body
                #:%put-failed-run)
  (:import-from #:cl-mock
                #:answer)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:multi-org-test-installation
                #:with-test-user)
  (:import-from #:alexandria
                #:assoc-value)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/test-failed-run)

(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    (with-installation (:installation (make-instance 'multi-org-test-installation))
     (with-test-store ()
       (&body)))))

(test simple-put-list
  (with-fixture state ()
    (let ((failed-run-dto (make-instance 'dto:failed-run
                                         :channel "bleh"
                                         :commit "foo")))
      (answer (parse-body 'dto:failed-run)
        failed-run-dto)
      (with-test-user (:logged-in-p t)
        (finishes
          (%put-failed-run))
        (is (eql 1 (length (%list-failed-runs))))
        (is (equal "bleh" (dto:failed-run-channel (car (%list-failed-runs))))))
      (with-test-user (:logged-in-p t :company-name "two")
        (is (eql 0 (length (%list-failed-runs))))))))

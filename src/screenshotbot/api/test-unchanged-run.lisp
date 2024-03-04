;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/test-unchanged-run
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/api/unchanged-run
                #:%post-unchanged-run
                #:parse-body)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:cl-mock
                #:answer)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:unchanged-run-for-commit
                #:unchanged-run-other-commit)
  (:import-from #:screenshotbot/api/recorder-run
                #:*synchronous-promotion*)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/test-unchanged-run)


(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    (with-test-store ()
      (let ((*synchronous-promotion* t))
       (&body)))))


(test simple-create-api
  (with-fixture state ()
    (let ((unchanged-run-dto (make-instance 'dto:unchanged-run
                                            :channel "bleh"
                                            :commit "foo"
                                            :other-commit "bar")))
      (answer (parse-body 'dto:unchanged-run)
        unchanged-run-dto)
      (with-test-user (:logged-in-p t :company company)
        (finishes
          (%post-unchanged-run)))
      (let ((ur (unchanged-run-for-commit
                  (car (bknr.datastore:class-instances 'channel))
                  "foo")))
        (is (not (null ur)))
        (is (equal "bar" (unchanged-run-other-commit ur)))))))

(test create-unchanged-run-with-batch
  (with-fixture state ()
    (let ((unchanged-run-dto (make-instance 'dto:unchanged-run
                                            :channel "bleh"
                                            :batch "foobar"
                                            :commit "foo"
                                            :other-commit "bar")))
      (answer (parse-body 'dto:unchanged-run)
        unchanged-run-dto)
      (with-test-user (:logged-in-p t :company company)
        (finishes
          (%post-unchanged-run)))
      (let ((ur (unchanged-run-for-commit
                  (car (bknr.datastore:class-instances 'channel))
                  "foo")))
        (is (not (null ur)))
        (is (equal "bar" (unchanged-run-other-commit ur)))))))

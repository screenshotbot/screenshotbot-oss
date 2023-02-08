;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-abstract-pr-promoter
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:retrieve-run
                #:run-retriever)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:util/testing
                #:with-global-binding))
(in-package :screenshotbot/test-abstract-pr-promoter)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-global-binding ((lparallel:*kernel* (lparallel:make-kernel 2)))
     (let ((channel (make-instance 'channel)))
       (&body)))))

(test simple-run-retriever-test
  (with-fixture state ()
    (let ((retriever (make-instance 'run-retriever
                                    :sleep-fn #'identity)))
      (is (equal nil (lparallel:force (retrieve-run retriever channel "abcd")))))))

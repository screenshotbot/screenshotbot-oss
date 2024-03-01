;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-unchanged-run
  (:use #:cl
        #:fiveam)
  (:import-from #:cl-mock
                #:answer
                #:with-mocks)
  (:import-from #:screenshotbot/sdk/sdk
                #:request)
  (:import-from #:util/fake-clingon
                #:make-fake-clingon)
  (:import-from #:screenshotbot/sdk/unchanged-run
                #:all-args
                #:maybe-create-batch)
  (:import-from #:screenshotbot/sdk/run-context
                #:run-context)
  (:import-from #:screenshotbot/sdk/api-context
                #:remote-version))
(in-package :screenshotbot/sdk/test-unchanged-run)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-mocks ()
    (&body)))

(defclass fake-run-context (run-context)
  ((env :initarg :env)))

(test happy-path-create-patch ()
  (with-fixture state ()
    (let ((called nil))
      (cl-mock:if-called 'request
                         (lambda (api-ctx api &key method content)
                           (setf called t)))
      (answer (remote-version :api-context) 11)
      (maybe-create-batch
       :api-context
       (make-fake-clingon (all-args) :batch "foo" :commit "bleh" :channel "car")
       :run-context-type 'fake-run-context
       :env-reader nil)
      (is-true called))))

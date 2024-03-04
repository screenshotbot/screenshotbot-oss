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
                #:handle-cmd
                #:mark-unchanged-run
                #:all-args)
  (:import-from #:screenshotbot/sdk/run-context
                #:run-context)
  (:import-from #:screenshotbot/sdk/api-context
                #:remote-version)
  (:import-from #:screenshotbot/sdk/cli-common
                #:root-options
                #:make-api-context)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/test-unchanged-run)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-mocks ()
    (let ((called nil))
      (cl-mock:if-called 'make-api-context
                         (lambda (&rest args)
                           :fake-api-context))
      (cl-mock:if-called 'request
                         (lambda (api-ctx api &key method content)
                           (setf called content)))
      (answer (remote-version :fake-api-context) 11)
      (&body))))

(test mark-unchanged-run-happy-path ()
  (with-fixture state ()
    (mark-unchanged-run
     :fake-api-context
     :other-commit "abcd"
     :run-context (make-instance 'run-context
                                 :channel "bleh"))
    (is-true called)
    (is (equal "bleh" (dto:unchanged-run-channel called)))
    (is (equal "abcd" (dto:unchanged-run-other-commit called)))))

(test handle-cmd-happy-path
  (with-fixture state ()
    (handle-cmd (make-fake-clingon (append
                                    (root-options)
                                    (all-args))
                                   :channel "bleh"
                                   :commit "0001"
                                   :other-commit "0002"))))

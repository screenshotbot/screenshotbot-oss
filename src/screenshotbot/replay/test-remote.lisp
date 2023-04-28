;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-remote
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/replay/remote
                #:started-at
                #:finished-at
                #:remote-run-status
                #:remote-run)
  (:import-from #:bknr.datastore
                #:with-transaction))
(in-package :screenshotbot/replay/test-remote)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test setf-remote-run-status
  (with-fixture state ()
    (let ((run (make-instance 'remote-run)))
      (with-transaction ()
        (setf (remote-run-status run) :success))
      (is (> (finished-at run) 0)))))

(test setf-remote-run-status-running
  (with-fixture state ()
    (let ((run (make-instance 'remote-run)))
      (with-transaction ()
        (setf (remote-run-status run) :running))
      (is (> (started-at run) 0)))))

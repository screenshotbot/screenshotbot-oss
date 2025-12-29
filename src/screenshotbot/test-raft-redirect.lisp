;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-raft-redirect
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/raft-redirect
                #:exclude-from-raft-redirect-p)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/test-raft-redirect)

(util/fiveam:def-suite)

(test exclude-from-raft-redirect-p-excludes-raft-state
  (with-fake-request (:script-name "/raft-state")
    (is-true (exclude-from-raft-redirect-p hunchentoot:*request*)))
  (with-fake-request (:script-name "/raft-state/foo")
    (is-true (exclude-from-raft-redirect-p hunchentoot:*request*))))

(test exclude-from-raft-redirect-p-excludes-intern-rpc
  (with-fake-request (:script-name "/intern/rpc")
    (is-true (exclude-from-raft-redirect-p hunchentoot:*request*)))
  (with-fake-request (:script-name "/intern/rpc?foo=bar")
    (is-true (exclude-from-raft-redirect-p hunchentoot:*request*))))

(test exclude-from-raft-redirect-p-allows-other-paths
  (with-fake-request (:script-name "/")
    (is-false (exclude-from-raft-redirect-p hunchentoot:*request*)))
  (with-fake-request (:script-name "/api/runs")
    (is-false (exclude-from-raft-redirect-p hunchentoot:*request*)))
  (with-fake-request (:script-name "/intern/other")
    (is-false (exclude-from-raft-redirect-p hunchentoot:*request*)))
  (with-fake-request (:script-name "/raft-something-else")
    (is-false (exclude-from-raft-redirect-p hunchentoot:*request*))))

;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/test-pull-requests
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/insights/pull-requests
                #:canonical-pr-url
                #:safe-pr)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/insights/test-pull-requests)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test canonical-pr-url-is-nil-when-we-cant-canonicalize
  "A channel without a repo can't tell us the forge, so
GET-CANONICAL-PULL-REQUEST-URL falls back to its default method. We
must not use that default, since every such run would collapse into a
single \"#\" key."
  (with-fixture state ()
    (let ((run (make-recorder-run
                :channel (make-instance 'channel :name "channel-0")
                :pull-request "git@github.com:foo/bar.git/pull/42")))
      (is (eql nil (canonical-pr-url run)))
      (is (equal "git@github.com:foo/bar.git/pull/42"
                 (safe-pr run))))))

(test safe-pr-is-nil-without-a-pull-request
  (with-fixture state ()
    (let ((run (make-recorder-run
                :channel (make-instance 'channel :name "channel-0"))))
      (is (eql nil (safe-pr run))))))

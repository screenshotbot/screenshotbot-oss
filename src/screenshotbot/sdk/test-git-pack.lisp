;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-git-pack
  (:use #:cl
        #:fiveam)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:starts-with)
  (:import-from #:screenshotbot/sdk/git-pack
                #:make-upload-pack-command))
(in-package :screenshotbot/sdk/test-git-pack)

(util/fiveam:def-suite)

(test parse-remote-git-pack
  (assert-that
   (make-upload-pack-command "/tmp/foo/bar.git")
   (starts-with "git upload-pack"))
  (assert-that
   (make-upload-pack-command "ssh://user@host:/tmp/foo.git")
   (starts-with "ssh user@host git upload-pack "))
  (assert-that
   (make-upload-pack-command "ssh://user@foo.host.com:/tmp/foo.git")
   (starts-with "ssh user@foo.host.com git upload-pack "))
  (assert-that
   (make-upload-pack-command "ssh://user@foo.host.com:tmp/foo.git")
   (starts-with "ssh user@foo.host.com git upload-pack tmp/foo.git"))  )



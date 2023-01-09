;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-git
            (:use #:cl
                  #:fiveam)
            (:import-from #:screenshotbot/sdk/git
                          #:assert-commit)
            (:import-from #:fiveam-matchers/core
                          #:assert-that)
            (:import-from #:fiveam-matchers/strings
                          #:contains-string))
(in-package :screenshotbot/sdk/test-git)

(util/fiveam:def-suite)

(test assert-commit
  (handler-case
      (progn
        (assert-commit "foo" "foo bar")
        (fail "expected error"))
    (error (e)
      (assert-that (format nil "~a" e)
                   (contains-string "`foo` does not")))))

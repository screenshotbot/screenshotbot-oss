;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-mail
  (:use #:cl
        #:fiveam)
  (:import-from #:util
                #:token-safe-for-email-p))
(in-package :util/tests/test-mail)

(util/fiveam:def-suite)

(test token-safe-for-email-p
  (is-true (token-safe-for-email-p "this is a test"))
  (is-false (token-safe-for-email-p "www.google.com"))
  (is-false (token-safe-for-email-p "this is https://google.com a test")))

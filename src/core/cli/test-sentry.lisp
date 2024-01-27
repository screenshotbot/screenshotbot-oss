;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/cli/test-sentry
  (:use #:cl
        #:fiveam)
  (:import-from #:core/cli/sentry
                #:with-cli-sentry))
(in-package :core/cli/test-sentry)

(util/fiveam:def-suite)

(define-condition my-error (error)
  ())

(test happy-path
  (finishes
    (with-cli-sentry (:dry-run t
                      :on-error (lambda ()))
      (+ 1 1)))
  (signals my-error
    (with-cli-sentry (:dry-run t
                      :on-error (lambda ()))
      (error 'my-error))))

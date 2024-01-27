;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-sentry
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/sentry
                #:with-sentry)
  (:import-from #:util/threading
                #:*tags*
                #:*extras*))
(in-package :screenshotbot/sdk/test-sentry)

(util/fiveam:def-suite)

(define-condition my-error (error)
  ())

(test happy-path
  (finishes
    (with-sentry (:dry-run t
                      :on-error (lambda ()))
      (+ 1 1)))
  (signals my-error
    (with-sentry (:dry-run t
                  :on-error (lambda ()))
      (error 'my-error))))

(test all-tags-and-extras-evaluate
  (finishes
    (with-sentry (:dry-run t
                  :on-error (lambda ()))
      (loop for fn in *extras*
            do (funcall fn (make-condition 'my-error)))
      (loop for fn in *tags*
            do (funcall fn (make-condition 'my-error))))))

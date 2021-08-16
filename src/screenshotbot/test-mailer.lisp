;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/test-mailer
    (:use #:cl
          #:fiveam
          #:alexandria)
  (:import-from #:screenshotbot/mailer
                #:port
                #:host
                #:local-smtp-mailer)
  (:import-from #:fiveam-matchers
                #:equal-to
                #:assert-that
                #:is-equal-to))


(util/fiveam:def-suite)

(test local-smtp-mailer
  (let ((mailer (make-instance 'local-smtp-mailer)))
    (assert-that (host mailer)
                 (equal-to "localhost"))
    (assert-that (port mailer)
                 (equal-to 25))))

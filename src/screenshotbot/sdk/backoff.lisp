;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/backoff
  (:use #:cl))
(in-package :screenshotbot/sdk/backoff)

(defun backoff (num)
  "A backoff for auto-restart"
  (let ((ret (car (nthcdr num (list 5 10 60)))))
    (warn "Will retry in ~a seconds" ret)
    ret))

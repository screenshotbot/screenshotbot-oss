;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/failed-run
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/git
                #:current-commit
                #:git-repo)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:version-check #:screenshotbot/sdk/version-check)
                    (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/failed-run)

(defun mark-failed ()
  (when (< version-check:*remote-version* 3)
    (error "The remote Screenshotbot server does not support --mark-failed"))

  (when (equal flags:*channel* "unnamed-channel")
    (error "You must provide a --channel to use --mark-failed"))

  (let ((repo (git-repo)))
    (sdk:request "/api/failed-run" :method :put
                                   :content (make-instance 'dto:failed-run
                                                           :commit (current-commit repo)
                                                           :channel flags:*channel*))))

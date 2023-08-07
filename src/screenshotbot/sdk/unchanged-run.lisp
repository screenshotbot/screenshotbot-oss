;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/unchanged-run
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/git
                #:current-commit
                #:git-repo)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:version-check #:screenshotbot/sdk/version-check)
                    (#:dto #:screenshotbot/api/model))
  (:export
   #:mark-unchanged-run))
(in-package :screenshotbot/sdk/unchanged-run)

(defun mark-unchanged-run ()
  (when (< version-check:*remote-version* 6)
    (error "The remote Screenshotbot server does not support --mark-unchanged-from"))

  (when (equal flags:*channel* "unnamed-channel")
    (error "You must provide a --channel to use --mark-unchanged-from"))

  (let ((repo (git-repo)))
    (sdk:request "/api/unchanged-run"
                 :method :post
                 :content
                 (make-instance 'dto:unchanged-run
                                :commit (current-commit repo)
                                :channel flags:*channel*
                                :other-commit flags:*unchanged-from*))))

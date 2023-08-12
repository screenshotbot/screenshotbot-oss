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
  (:import-from #:screenshotbot/sdk/api-context
                #:remote-version)
  (:import-from #:screenshotbot/sdk/cli-common
                #:register-root-command
                #:with-clingon-api-context)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:clingon.options
                #:make-option)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:version-check #:screenshotbot/sdk/version-check)
                    (#:dto #:screenshotbot/api/model))
  (:export
   #:mark-unchanged-run))
(in-package :screenshotbot/sdk/unchanged-run)

(defun mark-unchanged-run (api-context &key
                                         commit
                                         (channel flags:*channel*)
                                         (other-commit flags:*unchanged-from*))
  (when (< (remote-version api-context) 6)
    (error "The remote Screenshotbot server does not support --mark-unchanged-from"))

  (when (equal flags:*channel* "unnamed-channel")
    (error "You must provide a --channel to use --mark-unchanged-from"))

  (let* ((repo (git-repo))
         (commit (or commit (current-commit repo))))
    (sdk:request api-context
                 "/api/unchanged-run"
                 :method :post
                 :content
                 (make-instance 'dto:unchanged-run
                                :commit commit
                                :channel channel
                                :other-commit other-commit))))

(defun mark-unchanged-from/command ()
  (clingon:make-command
   :name "mark-unchanged"
   :description "Mark a commit as having identical screenshots to another commit on the same channel."
   :options (list
             (make-option
              :string
              :long-name "commit"
              :description "The current commit, defaults to the commit of the current repository"
              :key :commit)
             (make-option
              :string
              :long-name "other-commit"
              :description "The other commit that we're identifying this commit to be unchanged from"
              :key :other-commit)
             (make-option
              :string
              :long-name "channel"
              :description "The channel name"
              :key :channel))
   :handler (lambda (cmd)
              (with-clingon-api-context (api-context cmd)
                (apply #'mark-unchanged-run
                 api-context
                 (loop for key in `(:commit :other-commit :channel)
                       appending (list key (getopt cmd key))))))))

(register-root-command 'mark-unchanged-from/command)

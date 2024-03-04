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
  (:import-from #:screenshotbot/sdk/run-context
                #:flags-run-context
                #:env-reader-run-context)
  (:import-from #:screenshotbot/sdk/env
                #:make-env-reader)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:version-check #:screenshotbot/sdk/version-check)
                    (#:run-context #:screenshotbot/sdk/run-context)
                    (#:dto #:screenshotbot/api/model))
  (:export
   #:mark-unchanged-run))
(in-package :screenshotbot/sdk/unchanged-run)

(defun mark-unchanged-run (api-context &key
                                         (run-context (make-instance 'flags-run-context
                                                                     :env (make-env-reader)))
                                         (other-commit flags:*unchanged-from*))
  (when (< (remote-version api-context) 6)
    (error "The remote Screenshotbot server does not support --mark-unchanged-from"))

  (when (equal (run-context:channel run-context) "unnamed-channel")
    (error "You must provide a --channel to use --mark-unchanged-from"))

  (log:info "Marking commit as unchanged")
  (let* ((commit (run-context:commit-hash run-context)))
    (sdk:request api-context
                 "/api/unchanged-run"
                 :method :post
                 :content
                 (make-instance 'dto:unchanged-run
                                :commit commit
                                :channel (run-context:channel run-context)
                                :other-commit other-commit
                                :work-branch (run-context:work-branch run-context)
                                :batch (run-context:batch run-context)
                                :github-repo (run-context:repo-url run-context)
                                :override-commit-hash (run-context:override-commit-hash run-context)
                                :pull-request (run-context:pull-request-url run-context)
                                :phabricator-diff-id (run-context:phabricator-diff-id run-context)))))

(defun suffix (arg)
  (format nil "~a This is currently only used if `--batch` is provided, and in most cases we can guess this automatically in your CI." arg))

(defclass unchanged-run-run-context (run-context:env-reader-run-context
                             run-context:run-context)
  ())

(defun batch-args ()
  (list
   (make-option
    :string
    :long-name "batch"
    :description "An optional batch name for batching runs."
    :key :batch)
   (make-option
    :string
    :long-name "repo-url"
    :description (suffix "The optional repository URL.")
    :key :repo-url)
   (make-option
    :string
    :long-name "pull-request"
    :description (suffix "The pull request URL.")
    :key :pull-request)
   (make-option
    :string
    :long-name "phabricator-diff-id"
    :description (suffix "A Diff ID for Phabricator.")
    :key :phabricator-diff-id)))

(defun all-args ()
  (list*
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
    :key :channel)
   (batch-args)))

(defun handle-cmd (cmd)
  (with-clingon-api-context (api-context cmd)
    (mark-unchanged-run
     api-context
     :other-commit (getopt cmd :other-commit)
     :run-context (make-instance 'unchanged-run-run-context
                                 :env (make-env-reader)
                                 :commit-hash (getopt cmd :commit)
                                 :channel (getopt cmd :channel)
                                 :repo-url (getopt cmd :repo-url)
                                 :batch (getopt cmd :batch)
                                 :pull-request-url (getopt cmd :pull-request)
                                 :phabricator-diff-id (getopt cmd :phabricator-diff-id)))))

(defun mark-unchanged-from/command ()
  (clingon:make-command
   :name "mark-unchanged"
   :description "Mark a commit as having identical screenshots to another commit on the same channel."
   :options (all-args)
   :handler #'handle-cmd))

(register-root-command 'mark-unchanged-from/command)

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
                                         commit
                                         (channel flags:*channel*)
                                         (other-commit flags:*unchanged-from*))
  (when (< (remote-version api-context) 6)
    (error "The remote Screenshotbot server does not support --mark-unchanged-from"))

  (when (equal channel "unnamed-channel")
    (error "You must provide a --channel to use --mark-unchanged-from"))

  (log:info "Marking commit as unchanged")
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

(defun suffix (arg)
  (format nil "~a. This is currently only used if `--batch` is provided, and in most cases we can guess this automatically in your CI." arg))

(defclass batch-run-context (run-context:env-reader-run-context
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
    :key :pull-request)))

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

(defun post-batch (api-context dto)
  (log:info "Creating batch `~a`" (dto:batch-name dto))
  (sdk:request api-context
               "/api/batch"
               :method :post
               :content dto))

(defun maybe-create-batch (api-context cmd &key (run-context-type 'batch-run-context)
                                             (env-reader (make-env-reader)))
  (cond
    ((< (remote-version api-context) 11)
     (warn "The remote Screenshotbot server does not support --batch for unchanged-run"))
    ((getopt cmd :batch)
     (let ((run-context (make-instance run-context-type
                                       :env env-reader
                                       :commit-hash (getopt cmd :commit)
                                       :batch (getopt cmd :batch)
                                       :repo-url (getopt cmd :repo-url)
                                       :pull-request-url (getopt cmd :pull-request))))
       (post-batch api-context
                   (make-instance 'dto:batch
                                  :github-repo (run-context:repo-url run-context)
                                  :commit (or
                                           (run-context:override-commit-hash run-context)
                                           (run-context:commit-hash run-context))
                                  :name (run-context:batch run-context)
                                  :pull-request (run-context:pull-request-url run-context)))))))

(defun mark-unchanged-from/command ()
  (clingon:make-command
   :name "mark-unchanged"
   :description "Mark a commit as having identical screenshots to another commit on the same channel."
   :options (all-args)
   :handler (lambda (cmd)
              (with-clingon-api-context (api-context cmd)
                (maybe-create-batch api-context cmd)
                (apply #'mark-unchanged-run
                 api-context
                 (loop for key in `(:commit :other-commit :channel)
                       appending (list key (getopt cmd key))))))))

(register-root-command 'mark-unchanged-from/command)

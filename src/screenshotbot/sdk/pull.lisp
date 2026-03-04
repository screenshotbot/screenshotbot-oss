;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/pull
  (:use #:cl)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:clingon.options
                #:make-option)
  (:import-from #:screenshotbot/sdk/clingon-api-context
                #:with-clingon-api-context)
  (:import-from #:screenshotbot/sdk/fetch-run
                #:save-runs-from-commit
                #:save-run)
  (:export
   #:pull/command))
(in-package :screenshotbot/sdk/pull)

(defun download-run/command (&key
                               (name "download-run")
                               (description "[DEPRECATED] Use `pull run` instead."))
  (clingon:make-command
   :name name
   :handler (lambda (cmd)
              (with-clingon-api-context (api-context cmd)
                (save-run
                 api-context
                 (getopt cmd :run-id)
                 :output
                 (format nil "~a/" (or
                                    (getopt cmd :output)
                                    (format nil "./~a" (getopt cmd :run-id))))
                 :channel (getopt cmd :channel)
                 :branch (getopt cmd :branch))))
   :description description
   :options (list
             (make-option
              :string
              :long-name "id"
              :initial-value nil
              :description "The ID of the run, this is the ID you see in https://screenshotbot.io/runs/<ID>. Be aware that you cannot use a report ID here."
              :key :run-id)
             (make-option
              :string
              :long-name "channel"
              :initial-value nil
              :key :channel
              :description "A channel name, to provide instead of the --id, which will be used to download the active channel")
             (make-option
              :string
              :long-name "branch"
              :initial-value nil
              :key :branch
              :description "The branch to disambiguate active runs on the channel. By default, we'll pick a branch that matches `main` or `master`.")
             (make-option
              :string
              :long-name "output"
              :initial-value nil
              :description "The output directory to save the images. If not present it will default to  ./<id>"
              :key :output))))

(defun run/command ()
  (download-run/command
   :name "run"
   :description "Use this to download a run and all of its images locally."))

(defun commit/command ()
  (clingon:make-command
   :name "commit"
   :handler #'commit/handler
   :options (list
             (make-option
              :string
              :long-name "output"
              :initial-value nil
              :key :output
              :description "The output directory to save the images. If not present it will default to ./<commit>"))
   :description "Pull all screenshots for a given commit"))

(defun commit/handler (cmd)
  (with-clingon-api-context (api-context cmd)
   (let ((args (clingon:command-arguments cmd)))
     (unless (equal 1 (length args))
       (error "You must provide one argument, which is the commit SHA"))
     (let ((sha (elt args 0)))
       (pull-commit api-context sha :output (clingon:getopt cmd :output))))))

(defun pull-commit (api-context sha &key output)
  (save-runs-from-commit api-context sha
                         :output (or output (format nil "./~a/" sha))))

(defun pull/command ()
  (clingon:make-command
   :name "pull"
   :handler (lambda (cmd)
              (clingon:print-usage-and-exit cmd t))
   :description "Commands to existing screenshots from Screenshotbot"
   :sub-commands (list
                  (run/command)
                  (commit/command))))




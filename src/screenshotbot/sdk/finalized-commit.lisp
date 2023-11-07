;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/finalized-commit
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/git
                #:current-commit
                #:git-repo)
  (:import-from #:screenshotbot/sdk/api-context
                #:remote-version)
  (:import-from #:screenshotbot/sdk/cli-common
                #:register-root-command
                #:*root-commands*
                #:with-clingon-api-context)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:clingon.options
                #:make-option)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:version-check #:screenshotbot/sdk/version-check)
                    (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/finalized-commit)

(defun finalize-commit (api-context &key commit)
  (when (< (remote-version api-context) 7)
    (error "The remote Screenshotbot server does not support --mark-unchanged-from"))

  (let* ((repo (git-repo))
         (commit (or
                  commit
                  (current-commit repo))))
    (sdk:request api-context
                 "/api/finalized-commit"
                 :method :post
                 :content
                 (make-instance 'dto:finalized-commit
                                :commit commit))
    (log:info "Finalized runs for commit ~a" commit)))

(defun finalize-commit/command ()
  (clingon:make-command
   :name "finalize"
   :description "'Finalize' a commit. This tells Screenshotbot that there will no more runs associated with this commit (even if the same commit exists in multiple forked repositories in the organization"
   :options (list
             (make-option
              :string
              :long-name "commit"
              :initial-value nil
              :description "The commit to finalize, but will default to the current commit on the current repository"
              :key :commit))
   :handler (lambda (cmd)
              (with-clingon-api-context (api-context cmd)
                (finalize-commit api-context
                                 :commit (getopt cmd :commit))))))

(register-root-command 'finalize-commit/command)

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
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:version-check #:screenshotbot/sdk/version-check)
                    (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/finalized-commit)

(defun finalize-commit (api-context)
  (when (< (remote-version api-context) 7)
    (error "The remote Screenshotbot server does not support --mark-unchanged-from"))

  (let* ((repo (git-repo))
         (commit (current-commit repo)))
    (sdk:request api-context
                 "/api/unchanged-run"
                 :method :post
                 :content
                 (make-instance 'dto:finalized-commit
                                :commit commit))
    (log:info "Finalized runs for commit ~a" commit)))

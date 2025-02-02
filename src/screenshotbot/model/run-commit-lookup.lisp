;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;;; Logic to lookup runs by commit. Effectively needs to be aware of
;;;; the commit graph and information from the runs (e.g. to figure
;;;; the SHA for a specific branch.

(defpackage :screenshotbot/model/run-commit-lookup
  (:use #:cl)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-commit))
(in-package :screenshotbot/model/run-commit-lookup)

(defun find-runs-by-commit (commit &key)
  (let ((runs (bknr.datastore:class-instances 'recorder-run)))
    (loop for run in runs
          if (str:starts-with-p commit (recorder-run-commit run))
            collect run)))




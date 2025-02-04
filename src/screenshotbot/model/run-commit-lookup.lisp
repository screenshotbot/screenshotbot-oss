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
                #:runs-for-company
                #:recorder-run)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-commit)
  (:import-from #:util/misc
                #:or-setf))
(in-package :screenshotbot/model/run-commit-lookup)

(defvar *cache* (make-hash-table :test #'equal
                                 #+lispworks #+lispworks
                                 :weak-kind :key))

(defun find-runs-by-commit (commit &key (company nil company-provided-p))
  "Find runs for a given commit prefix. If company is provided, then we
only search runs for the given company, otherwise we search all runs
globally.

TODO: we might want to make the global runs more explicit, like :all,
since it's only used for the admin panel."
  (or-setf
   (gethash (list commit company-provided-p company) *cache*)
   (let ((runs (cond
                 (company-provided-p
                  (fset:convert 'list
                                (runs-for-company company)))
                 (t
                  (bknr.datastore:class-instances 'recorder-run)))))
     (loop for run in runs
           if (str:starts-with-p commit (recorder-run-commit run))
             collect run))))




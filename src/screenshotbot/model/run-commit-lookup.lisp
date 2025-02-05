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
                #:github-repo
                #:runs-for-company
                #:recorder-run)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-channel
                #:recorder-run-commit)
  (:import-from #:util/misc
                #:or-setf))
(in-package :screenshotbot/model/run-commit-lookup)

(defvar *cache* (make-hash-table :test #'equal
                                 #+lispworks #+lispworks
                                 :weak-kind :key))

(defun find-runs-by-commit (commit &key (company (error "must provide company (or :all)"))
                                     repo)
  "Find runs for a given commit prefix and given company.

To search for all companies (i.e. only for internal dashboards),
provide a :company of :all.
"
  (when repo
    (check-type repo string))
  (when (and (not (eql :all company)) (not repo))
    (error "If you provide :company, you must also provide :repo"))
  (or-setf
   (gethash (list commit company repo) *cache*)
   (let ((runs (cond
                 ((eql :all company)
                  (bknr.datastore:class-instances 'recorder-run))
                 (company
                  (loop for run in (fset:convert 'list (runs-for-company company))
                        if (equal repo (github-repo (recorder-run-channel run)))
                          collect run)))))
     (loop for run in runs
           if (str:starts-with-p commit (recorder-run-commit run))
             collect run))))



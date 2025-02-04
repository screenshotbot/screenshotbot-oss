;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/compare-branches
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:util/form-errors
                #:with-error-builder)
  (:import-from #:screenshotbot/user-api
                #:channel-name
                #:recorder-run-channel
                #:recorder-run-commit)
  (:import-from #:screenshotbot/model/run-commit-lookup
                #:find-runs-by-commit)
  (:import-from #:screenshotbot/diff-report
                #:diff-report-title
                #:diff-report-run
                #:diff-report-empty-p
                #:make-diff-report)
  (:import-from #:util/store/object-id
                #:oid))
(in-package :screenshotbot/dashboard/compare-branches)

(named-readtables:in-readtable markup:syntax)

(defun %form ()
  (let ((action (nibble (sha1 sha2)
                  (%post :sha1 sha1 :sha2 sha2))))
    <simple-card-page form-action=action >
      <div class= "card-header">
        <h4>Compare branches or commits</h4>
      </div>
      <div class= "card-body">
        <div class= "mb-2">
          <label for= "sha1" class= "form-label" >
            First SHA/branch
          </label>
          <input type= "text" class= "form-control" id= "sha1" name= "sha1" placeholder= "abcdef0102" />
        </div>

        <div class= "mb-2">
          <label for= "sha2" class= "form-label" >
            Second SHA/branch
          </label>
          <input type= "text" class= "form-control" id= "sha2" name= "sha2" placeholder= "abcdef0102"/>
        </div>

      </div>
      <div class= "card-footer">
        <input type= "submit" value= "Compare" class= "btn btn-primary" />
        <a href= "/" class= "btn btn-secondary" >Cancel</a>
      </div>
    </simple-card-page>))

(defhandler (nil :uri "/compare-branches") ()
  (assert (gk:check :compare-branches (auth:current-company)))
  (%form))

(defun resolve-commits (company prefix)
  (fset:convert 'fset:set
                (mapcar #'recorder-run-commit
                        (find-runs-by-commit prefix :company company))))

(defun %post (&key sha1 sha2)
  (with-error-builder (:check check
                       :errors errors
                       :form-builder (%form)
                       :form-args (:sha1 sha1
                                   :sha2 sha2)
                       :success (%perform :sha1 sha1 :sha2 sha2))
    (macrolet ((both-check (expr message)
                 "Simple macro to run a check on both sha1 and sha2. Use SHA in the
                  expression instead of SHA1/SHA2"
                 `(progn
                    ,@(loop for key in '(sha1 sha2)
                            collect
                            `(check ,(intern (string key) :keyword)
                                    (let ((sha ,key))
                                      ,expr)
                                    ,message)))
                 ))
      (both-check (> (length sha) 0)
                  "Commit SHA should not be empty")
      (unless errors
        (both-check (<= (fset:size (resolve-commits (auth:current-company) sha)) 1)
                    "Prefix does not uniquely resolve to a commit"))
      (unless errors
        (both-check (eql (fset:size (resolve-commits (auth:current-company) sha)) 1)
                    "Could not find a commit with that prefix"))
      (unless errors
        (both-check
         (< (length (find-runs-by-commit sha :company (auth:current-company))) 1000)
         "Too many runs for that commit to compare")))
    
    (unless errors
      (check :sha2 (not (equal sha1 sha2))
             "The SHAs shouldn't be the same"))))

(defun remove-dup-runs (runs)
  (remove-duplicates runs
                     :key #'recorder-run-channel))

(defun find-run (runs channel)
  (let ((run (find channel runs :key #'recorder-run-channel)))
    (when run
      (auth:can-view! run))
    run))

(defun diff-report-channel (diff-report)
  (recorder-run-channel (diff-report-run diff-report)))

(defun %perform (&key sha1 sha2)
  (let* ((company (auth:current-company))
         (runs1 (remove-dup-runs (find-runs-by-commit sha1 :company company)))
         (runs2 (remove-dup-runs (find-runs-by-commit sha2 :company company)))
         (channels1 (mapcar #'recorder-run-channel runs1))
         (channels2 (mapcar #'recorder-run-channel runs2))
         (common-channels (intersection channels1 channels2))
         (added (set-difference channels1 channels2))
         (removed (set-difference channels2 channels1))
         (diff-reports (loop for channel in common-channels
                             collect
                             (make-diff-report
                              (find-run runs1 channel)
                              (find-run runs2 channel))))
         (changed-diff-reports (remove-if #'diff-report-empty-p diff-reports))
         (unchanged-diff-reports (remove-if-not #'diff-report-empty-p diff-reports)))
    (declare (ignore unchanged-diff-reports
                     added
                     removed))
    (flet ((list-channels (diff-reports)
             <table class= "table border table-striped table-hover" >
               ,@ (loop for diff-report in diff-reports
                        for channel = (diff-report-channel diff-report)
                        for href = (format nil "/runs/~a/compare/~a"
                                           (oid (find-run runs1 channel))
                                           (oid (find-run runs2 channel)))
                        collect
                        <tr>
                          <td>
                            <a href= href >,(channel-name channel)</a>
                          </td>
                          <td>
                            ,(diff-report-title diff-report)
                          </td>
                        </tr>)
             </table>))
      <app-template>
        <h4 class= "mt-2 mb-2" >Changed channels</h4>
        ,(list-channels changed-diff-reports)
      </app-template>)))

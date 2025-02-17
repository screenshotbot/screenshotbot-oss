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
                #:oid)
  (:import-from #:screenshotbot/model/channel
                #:repos-for-company)
  (:import-from #:util/throttler
                #:throttle!
                #:throttler))
(in-package :screenshotbot/dashboard/compare-branches)

(named-readtables:in-readtable markup:syntax)

(defvar *throttler* (make-instance 'throttler
                                   :tokens 100))

(defhandler (nil :uri "/compare-branches/search-sha") ()
  (json:encode-json-to-string
   #("foo" "bar")))

(defun maybe-parameter (param)
  (hunchentoot:parameter (str:downcase param)))

(defun %form ()
  (let* ((repos (repos-for-company (auth:current-company)))
         (action (nibble (sha1 sha2 repo-idx)
                  (%post :sha1 sha1 :sha2 sha2
                         :repo (elt repos (parse-integer repo-idx)))))
         (autocomplete "/compare-branches/search-sha"))
    <simple-card-page form-action=action >
      <div class= "card-header">
        <h4>Compare branches or commits</h4>
      </div>
      ,(unless (and (maybe-parameter :repo)
                    (maybe-parameter :sha1)
                    (maybe-parameter :sha2))
         <div class= "alert alert-info mb-3">
           We currently do not support branch names, but you can auto-fill commit information when linking from external tools. Use <tt>?repo=<em>repo</em>&sha1=<em>sha</em>&sha2=<em>sha</em></tt> to prefill this form.
         </div>)
      <div class= "">

        <div class= "mb-3">
          <label for= "repo" class= "form-label">
            Repository
          </label>
          <select class= "form-select" id= "repo" name= "repo-idx" >
            ,@ (loop for repo in repos
                     for idx from 0
                     collect <option value= idx >,(progn repo)</option>)
          </select>
        </div>
        <div class= "mb-2">
          <label for= "sha1" class= "form-label" >
            First SHA
          </label>
          <input type= "text" class= "form-control sha-autocomplete" id= "sha1" name= "sha1" placeholder= "abcdef0102" data-autocomplete=autocomplete value= (maybe-parameter :sha1) />
        </div>

        <div class= "mb-2">
          <label for= "sha2" class= "form-label" >
            Second SHA
          </label>
          <input type= "text" class= "form-control sha-autocomplete" id= "sha2" name= "sha2" placeholder= "abcdef0102"
                 data-autocomplete=autocomplete
                 value= (maybe-parameter :sha2) />
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

(defun resolve-commits (company prefix &key repo)
  (fset:convert 'fset:set
                (mapcar #'recorder-run-commit
                        (find-runs-by-commit prefix :company company :repo repo))))

(defun %post (&key sha1 sha2 repo)
  (throttle! *throttler* :key (auth:current-user))
  (with-error-builder (:check check
                       :errors errors
                       :form-builder (%form)
                       :form-args (:sha1 sha1
                                   :sha2 sha2)
                       :success (%perform :sha1 sha1 :sha2 sha2 :repo repo))
    (assert repo)
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
        (both-check (<= (fset:size (resolve-commits (auth:current-company) sha :repo repo)) 1)
                    "Prefix does not uniquely resolve to a commit"))
      (unless errors
        (both-check (eql (fset:size (resolve-commits (auth:current-company) sha :repo repo)) 1)
                    "Could not find a commit with that prefix"))
      (unless errors
        (both-check
         (< (length (find-runs-by-commit sha
                                         :company (auth:current-company)
                                         :repo repo))
            1000)
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

(defun %perform (&key sha1 sha2 repo)
  (let* ((company (auth:current-company))
         (runs1 (remove-dup-runs (find-runs-by-commit sha1 :company company :repo repo)))
         (runs2 (remove-dup-runs (find-runs-by-commit sha2 :company company :repo repo)))
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

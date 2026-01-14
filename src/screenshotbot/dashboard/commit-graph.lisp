;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/commit-graph
  (:use #:cl
        #:alexandria
        #:markup
        #:screenshotbot/template)
  (:import-from #:screenshotbot/model/commit-graph
                #:commit-graph-dag)
  (:import-from #:screenshotbot/git-repo
                #:commit-graph)
  (:import-from #:dag
                #:ordered-commits)
  (:import-from #:util/timeago
                #:timeago)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:nibble
                #:nibble)
  (:export
   #:view-git-graph))
(in-package :screenshotbot/dashboard/commit-graph)

(named-readtables:in-readtable markup:syntax)

(defun %draw-graph (commit-graph)
  (let* ((input (with-output-to-string (s)
                  (loop for commit in (ordered-commits commit-graph)
                        do (format s "~a~{ ~a~}~%"
                                   (dag:sha commit)
                                   (dag:parents commit)))))
         (output (uiop:run-program (list (namestring (asdf:system-relative-pathname :screenshotbot "dashboard/git-graph")))
                           :input (make-string-input-stream input)
                           :output :string)))
    <app-template>
      <pre>,(progn output)</pre>
    </app-template>))

(deftag view-git-graph (repo)
  (let* ((commit-graph (commit-graph-dag (commit-graph (car repo))))
         (commits (dag:ordered-commits commit-graph)))
    <app-template>
      <div class= "alert alert-info mt-3">
        This shows all the information we have about your Git commit
 history. In particular, we only store the Git hashes. This
 information here is for debugging information only when reaching out
 to Screenshotbot support. (ID ,(progn (bknr.datastore:store-object-id (commit-graph (car repo)))))

        <a href= (nibble () (%draw-graph commit-graph) )> Draw Graph </a>
      </div>
      <table class= "table git-graph" >
        <thead>
          <tr>
            <th>Commit hash</th>
            <th>Parents</th>
            <th>First seen</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
        ,@ (loop for commit in commits collect
                 <tr id= (dag:sha commit) >
                   <td class= "font-monospace" >,(str:shorten 13 (dag:sha commit)) </td>
                   <td class= "font-monospace" >
                     ,@ (loop for parent in (dag:parents commit)
                              collect
                              <span>
                                <a href= (format nil "#~a" parent) class= "commit-link" >,(str:shorten 13 parent)</a>
                              </span>)

                   </td>
                   <td>
                     <timeago timestamp= (ignore-errors (dag:commit-timestamp commit)) />
                   </td>
                   <td><button class= "highlight-branch btn btn-link" data-commit= (dag:sha commit) >Highlight Branch</button></td>
                 </tr>)
        </tbody>
      </table>
    </app-template>))

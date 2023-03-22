;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/run-page
  (:use #:cl
        #:alexandria
        #:markup
        #:nibble
        #:screenshotbot/promote-api
        #:screenshotbot/report-api
        #:screenshotbot/user-api
        #:screenshotbot/git-repo
        #:screenshotbot/screenshot-api
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/view
        #:screenshotbot/model/screenshot
        #:screenshotbot/template
        #:screenshotbot/model/image
        #:screenshotbot/model/channel
        #:screenshotbot/model/company)
  (:import-from #:screenshotbot/server
                #:with-login
                #:make-thread
                #:defhandler)
  (:import-from #:util
                #:oid
                #:find-by-oid)
  (:import-from #:hex #:make-url)
  (:import-from #:screenshotbot/ui
                #:ui/a
                #:ui/div)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:core/ui/paginated
                #:paginated)
  (:import-from #:screenshotbot/model/report
                #:reports-for-run)
  (:import-from #:screenshotbot/report-api
                #:report-title)
  (:import-from #:screenshotbot/model/image
                #:image-dimensions)
  (:import-from #:screenshotbot/model/recorder-run
                #:compare-threshold
                #:compared-against
                #:merge-base-failed-warning
                #:recorder-run-warnings
                #:override-commit-hash)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/git-repo
                #:commit-graph)
  (:import-from #:dag
                #:ordered-commits)
  (:import-from #:screenshotbot/ui/confirmation-page
                #:confirmation-page)
  (:import-from #:screenshotbot/user-api
                #:adminp)
  (:import-from #:screenshotbot/model/company
                #:company-admins)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/model/view
                #:can-edit)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:screenshotbot/dashboard/review-link
                #:review-link
                #:describe-pull-request)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:export
   #:*create-issue-popup*
   #:run-page
   #:run-row-filter
   #:row-filter
   #:commit)
  (:export #:mask-editor
           #:start-review-enabled-p))
(in-package :screenshotbot/dashboard/run-page)

(named-readtables:in-readtable markup:syntax)

(defvar *create-issue-popup* nil
  "On the non-OSS Screenshotbot, we have a feature to Jira tasks
  directly from Screenshotbot by annotating images. This is just a
  reference to that function. If there are requests for it, we'll open
  source it too.")

(deftag commit (&key repo hash)
  (ui/a :href (commit-link repo hash)
    (str:substring 0 8 hash)))

(defhandler (run-page :uri "/runs/:id" :method :get) (id name)
  (let* ((run (find-by-oid id 'recorder-run)))
    (flet ((render ()
             (render-run-page run :name name)))
     (cond
       ((can-public-view run)
        (render))
       (t
        (with-login (:company (recorder-run-company run))
          (render)))))))

(deftag page-nav-dropdown (children &key title)
  (let ()

    (mquery:add-class (mquery:$ "a" children) "dropdown-item")

    <div class="dropdown">
  <button class="btn btn-sm btn-secondary dropdown-toggle" type="button" id="dropdownMenuButton" data-bs-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
    ,(progn title)
  </button>
  <div class="dropdown-menu" aria-labelledby="dropdownMenuButton" style= "z-index: 99999999; position: static" >
    ,@(progn children)
  </div>
    </div>))

(defun promotion-log-page (run)
  <app-template>
  <section>
  <pre>
  --BEGIN-LOGS--
,(let ((file (bknr.datastore:blob-pathname (promotion-log run))))
      (cond
        ((path:-e file)
         (uiop:read-file-string file))
        (t
         "Log file unavailable")))
  --END-LOGS--
  </pre>
  </section>
  </app-template>)

(defun run-size (run)
  "Get the total size of all the screenshots associated with this run"
  (floor
   (loop for screenshot in (recorder-run-screenshots run)
         summing
         (with-local-image (file screenshot)
           (trivial-file-size:file-size-in-octets file)))
   1024))

(defun screenshots-above-16k-dim (run)
  "Get the number of screenshots that have a dimension above 16k. This
  is the limit imposed by webp images."
  (let ((+limit+ 16383))
    (loop for screenshot in (recorder-run-screenshots run)
          for dim = (image-dimensions screenshot)
          if (or
              (> (dimension-height dim) +limit+)
              (> (dimension-width dim) +limit+))
            collect screenshot)))

(deftag view-git-graph (repo)
  (let* ((commit-graph (commit-graph-dag (commit-graph (car repo))))
         (commits (dag:ordered-commits commit-graph)))
    <app-template>
      <div class= "alert alert-info mt-3">
        This shows all the information we have about your Git commit
 history. In particular, we only store the Git hashes. This
 information here is for debugging information only when reaching out
 to Screenshotbot support.
      </div>
      <table class= "table git-graph" >
        <thead>
          <tr>
            <th>Commit hash</th>
            <th>Author (if available)</th>
            <th>Parents</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
        ,@ (loop for commit in commits collect
                 <tr id= (dag:sha commit) >
                   <td class= "font-monospace" >,(str:shorten 13 (dag:sha commit)) </td>
                   <td>,(or (dag:author commit) "no author") </td>
                   <td class= "font-monospace" >
                     ,@ (loop for parent in (dag:parents commit)
                              collect
                              <span>
                                <a href= (format nil "#~a" parent) class= "commit-link" >,(str:shorten 13 parent)</a>
                              </span>)

                   </td>
                   <td><button class= "highlight-branch btn btn-link" data-commit= (dag:sha commit) >Highlight Branch</button></td>
                 </tr>)
        </tbody>
      </table>
    </app-template>))

(defun unpromote-run-flow (run)
  (confirmation-page
   :yes (nibble ()
          (unpromote-run run)
          (hex:safe-redirect (nibble () (advanced-run-page :run run
                                                           :alert "This run has been un-promoted"))))
   :no (nibble ()
         (advanced-run-page :run run))

     <span>
       Are you sure you want to unpromote this run? Usually you only have to do this if you rewrite your Git history. If you're unsure please reach out to Screenshotbot support before doing this.
     </span>
))


(deftag advanced-run-page (&key run alert)
  (let ((repo (channel-repo (recorder-run-channel run))))
    <simple-card-page max-width= "40rem" >
        <div class="card-header">
          <h3>Debug Run Information</h3>
        </div>
        <div class="card-body">
          ,(when alert
             <div class="alert alert-info mt-3">,(progn alert)</div>)

          <ul>
            <li>Repo url: ,(github-repo run)</li>
            <li>Commit: ,(commit :repo repo :hash (recorder-run-commit run)) </li>
            <li>Override Commit hash for Pull Requests: ,(commit :repo repo :hash (override-commit-hash run))</li>
            <li>Main Branch: ,(recorder-run-branch run)</li>
            <li>Commit on branch: ,(commit :repo repo :hash (ignore-errors
                                                             (recorder-run-branch-hash run)))</li>
            <li>Merge base: ,(commit :repo repo :hash (recorder-run-merge-base run))</li>
            <li>Pull request: ,(or (pull-request-url run) "NA")</li>
            <li>Phabricator Diff-id: ,(or (phabricator-diff-id run) "NA")</li>
            <li>Build URL: <a href=(run-build-url run)>,(run-build-url run)</a> </li>
            <li>Production?: ,(if (trunkp run) "true" "false")</li>
            <li>Periodic job?: ,(if (periodic-job-p run) "true" "false")</li>
            <li>Number of screenshots: ,(length (recorder-run-screenshots run))</li>
            <li>Total run size: ,(run-size run)kB</li>
            <li>Comparison threshold: ,(compare-threshold run)</li>

            <li>Screenshots that are above 16k dimensions: ,(length (screenshots-above-16k-dim run))

            <a href= (nibble ()
                               <app-template>
                                 List of screenshots with large dimensions
                                 <ul>
                                   ,@(loop for x in (screenshots-above-16k-dim run)
                                           collect <li>
                                     <a href= (make-url 'run-page :id (oid run) :name (screenshot-name x))>
                                       ,(screenshot-name x)
                                                   </a></li>)
                                 </ul>
                               </app-template>)>
                               List
    </a>
            </li>
          </ul>
        </div>

    <div class="card-footer">
      <form>
        <a href= (nibble () (view-git-graph repo))
           class= "btn btn-secondary">Debug Git Graph</a>
        ,(when (and (activep run)
                    (or
                     (adminp (current-user))
                     (member (current-user) (company-admins (current-company)))))
           <a
             href=(nibble () (unpromote-run-flow run))
             class="btn btn-danger" >

             Undo promotion
           </a>)
      </form>
        </div>

    </simple-card-page>))

(defmethod extra-advanced-options (run)
  )

(defmethod start-review-enabled-p ((installation t) (run t))
  nil)

(deftag run-advanced-menu (&key run)
  (let ((promotion-logs (nibble ()
                          (promotion-log-page run)))
        (rerun-promotions (nibble ()
                            (make-thread
                             (lambda ()
                               (log:info "Starting re-run promotion thread")
                               (start-promotion-thread
                                run)))
                            (hex:safe-redirect 'run-page
                                                :id (oid run))))
        (debug-info (nibble ()
                      (advanced-run-page :run run))))
    <page-nav-dropdown title= "Advanced">
      <a href= promotion-logs >Promotion Logs</a>
      <a href= rerun-promotions >Re-Run Promotions</a>
      <a href=debug-info >Debug Info</a>
      ,(when (start-review-enabled-p (installation) run)
         <a href= (format nil "/review/~a" (oid run)) >Start Review</a>)
    </page-nav-dropdown>))

(defun create-filter-matcher (filter &key key)
  (cond
    ((eq t filter)
     'identity)
    (t (lambda (x) (equal (funcall key x)
                          filter)))))

(defclass row-filter ()
  ((key :initarg :key
        :initform 'identity
        :accessor row-filter-key)
   (value :initarg :value
          :accessor row-filter-value)))

(defun run-row-filter (row-filter list)
  (let ((list (if (listp list) list (list list))))
   (let ((filter (row-filter-value row-filter)))
     (loop for x in list
           if (or
               (eq filter t)
               (equal filter (funcall (row-filter-key row-filter) x)))
             collect x))))

(deftag warning-alert (children)
  <div class= "alert alert-warning mt-2">
    <strong class= "pe-1" >Caution!</strong>
    ,@children
  </div>)

(defmethod render-run-warning (run (warning merge-base-failed-warning))
  (let ((repo (channel-repo (recorder-run-channel run))))
   (flet ((link (hash)
            (commit-link repo
                         hash)))
     <warning-alert>
       <span>The <a href= (link (recorder-run-commit run))>merge base</a> for ,(review-link :run run) had a failing build. Screenshotbot used <a href= (link (recorder-run-commit (compared-against warning)))>this commit</a> to generate reports for this run. Consider rebasing to avoid this message.
       </span>
     </warning-alert>)))

(defmethod render-run-warning (run (warning not-fast-forward-promotion-warning))
  (when-let ((previous-run (recorder-previous-run run)))
    <warning-alert>
      <span>
        This run was not a fast-forward of the commit from the <a href= (run-link previous-run)>previous promoted run</a>. This might be the result of rewriting Git history with <tt>git push -f</tt>, or because of an incorrect invocation of the CLI tool on a developer device.
      </span>
    </warning-alert>))

(defun render-warnings (run)
  (when-let ((warnings (recorder-run-warnings run)))
    (loop for warning in warnings
          collect
          (render-run-warning run warning))))



(defun render-run-page (run &rest filters &key name)
  (can-view! run)
  (flet ((re-call (&rest args)
           (apply 'render-run-page run (append args filters))))

    (let* ((channel (recorder-run-channel run))
           (screenshots (recorder-run-screenshots run))
           (filtered-screenshots (cond
                                   (name
                                    (loop for s in screenshots
                                          if (string-equal (screenshot-name s) name)
                                            collect s))
                                   (t
                                    screenshots))))
      <app-template body-class= "dashboard bg-white" >
        <div class= "page-title-box">
          <h4 class= "page-title" >Run from
            <:time class= "timeago" datetime= (created-at run)>
              ,(created-at run)
            </:time>
          </h4>

          <div class= "d-flex justify-content-between mt-3 mb-3">
            <div class= "" style= "width: 20em" >
              <div class= "input-group">
                <span class= "input-group-text" >
                  <mdi name= "search" />
                </span>
                <input class= "form-control search d-inline-block" type= "text" autocomplete= "off"
                       placeholder= "Search..."
                       data-target= "#run-page-results" />
              </div>
            </div>


            ,(when (can-edit run (current-user))
               <div class= "">
                 <run-advanced-menu run=run />
               </div>)
          </div>

        </div>

          ,@(render-warnings run)

          ,(when-let (reports (reports-for-run run))
             <div class= "alert alert-info mt-2" >
             This run created a report with

               ,(let ((report (car reports)))
                          <a href= (format nil "/report/~a" (oid report)) >,(report-title report)</a>)
             </div>)

          <div id= "run-page-results" class= "search-results" data-update= (nibble () (update-content run channel))
               data-args= "{}" >
            ,(run-page-contents run channel filtered-screenshots)
          </div>

      </app-template>)))

(defun update-content (run channel)
  (let* ((query (hunchentoot:parameter "search"))
         (screenshots (recorder-run-screenshots run)))
    (run-page-contents
     run channel
     screenshots
     :filter (lambda (screenshot)
               (or (null query)
                   (str:contains? query (screenshot-name screenshot)
      :ignore-case t))))))

(defun run-link (run)
  (make-url 'run-page :id (oid run)))

(defun make-id ()
  (format nil "a-~a" (random 10000000)))

(defclass screenshots-viewer ()
  ((screenshots :initarg :screenshots
                :reader screenshots-viewer-screenshots)
   (filter :initarg :filter
           :initform #'identity
           :reader screenshots-viewer-filter)
   (navigationp :initarg :navigationp
                :initform t
                :documentation "Whether to show the Prev/Next bar"
                :reader navigationp)
   (mapper :initarg :mapper
           :initform #'identity
           :reader screenshots-viewer-mapper
           :documentation "map the object in the list to get the screenshot")
   (modal-id :initform (make-id)
             :reader modal-id)))

(defmethod filtered-screenshots ((self screenshots-viewer))
  (remove-if-not (screenshots-viewer-filter self)
                 (screenshots-viewer-screenshots self)))

(defmethod render-modal ((self screenshots-viewer))
  (let ((get-ith-image (nibble (n)
                         (setf (hunchentoot:content-type*) "application/json")
                         (let ((screenshot (funcall
                                            (screenshots-viewer-mapper self)
                                            (elt (filtered-screenshots self) (parse-integer n)))))
                           (json:encode-json-to-string
                            `((:src . ,(image-public-url
                                        (screenshot-image screenshot)
                                        ;; todo: full-page
                                        :type :webp))
                              (:title . ,(screenshot-name screenshot))))))))
    <div class= "modal fade single-screenshot-modal" id= (modal-id self) tabindex= "-1" role= "dialog"
         aria-hidden= "true" >
      <div class= "modal-dialog modal-fullscreen " role= "document">
        <div class= "modal-content">
          <div class= "modal-header">
            <h5 class= "modal-title"></h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>

          <div class= "modal-body">
            <div class= "modal-body-content">
              ,(when (navigationp self)
                 <div class= "d-flex justify-content-between mb-2 align-items-center content-header">
                   <a href= "#" class= "btn previous"><mdi name= "navigate_before"/>Previous</a>
                   <span class= "page-num" />
                   <a href= "#" class= "btn next">Next<mdi name= "navigate_next" /></a>
                 </div>)
              <div class= "canvas-container "
                   data-length= (length (filtered-screenshots self))
                   data-src=get-ith-image >
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>))


(defun run-page-contents (run channel screenshots &key (filter #'identity))
  (let ((screenshots-viewer (make-instance 'screenshots-viewer
                                           :screenshots screenshots
                                           :filter filter)))
   <div id= (make-id) >
     ,(render-modal screenshots-viewer)
     ,(paginated
       (lambda (screenshot i)
         (let* ((name-parts (str:rsplit "--" (screenshot-name screenshot) :limit 2)))
           <div class= " col-sm-12 col-md-4 col-lg-3 mb-1 mt-2">
             <div class="card">
               <div class="card-body">
                 <div class= "screenshot-header" >
                   <h4 class= "screenshot-title" >,(car name-parts)</h4>
                   ,(when (cadr name-parts)
                      <h6>,(cadr name-parts)</h6>)
                   <ul class= "screenshot-options-menu">
                     <li>
                       <a href= (make-url 'history-page :channel (store-object-id channel)
                                                                                          :screenshot-name (screenshot-name screenshot))
                          >
                         History
                       </a>
                     </li>

                     <li>
                       <a href= (nibble () (mask-editor (recorder-run-channel run) screenshot
                          :redirect (run-link run)))
                          >Edit Masks</a>

                     </li>

                     ,(when *create-issue-popup*
                        <li>
                          <a target= "_blank"
                             href= (nibble ()
                                             (funcall *create-issue-popup* run screenshot)) >
                            Create Issue
                          </a>
                        </li>)
                   </ul>
                 </div>
                 <a href= (image-public-url (screenshot-image screenshot) :size :full-page :type "webp") title= (screenshot-name screenshot)
                    class= "screenshot-run-image"
                    data-image-number=i
                    data-target= (format nil "#~a" (modal-id screenshots-viewer))>
                   ,(let ((dimensions (ignore-errors (image-dimensions (screenshot-image screenshot)))))
                      <picture class="">
                        <source srcset= (image-public-url (screenshot-image screenshot) :size :small :type :webp) />
                        <:img
                          class= "screenshot-image run-page-image"
                          src= (image-public-url (screenshot-image screenshot)  :size :small
                                                                              :type :png)
                          width= (?. dimension-width dimensions)
                          height= (?. dimension-height dimensions)
                          />
                      </picture>)
                 </a>
               </div> <!-- end card-body-->
             </div>

           </div>))
       :pass-index-p t
       :items screenshots
       :filter filter
       :empty-view  <p class= "text-muted" >No screenshots found</p>)
   </div>))

(defclass js-api-result () ())

(defclass js-api-success (js-api-result)
  ((success :type boolean
            :initform t)
   (was-promoted :type boolean
                 :initarg :was-promoted
                 :initform nil)))

(defhandler (run-delete-page :uri "/runs/:id" :method :delete) (id)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((run (find-by-oid id 'recorder-run)))
    (with-login (:company (recorder-run-company run))
      (can-view! run)
      (cond
        ((or (activep run)
             (recorder-previous-run run))
         (log:info "Can't delete: ~s this run seems to be a master run" run)
         (json:encode-json-to-string (make-instance 'js-api-success
                                                     :was-promoted t)))
        (t
         (bknr.datastore:with-transaction ()
           (setf (company-runs (current-company))
                 (remove run (company-runs (current-company)))))
         (json:encode-json-to-string (make-instance 'js-api-success)))))))

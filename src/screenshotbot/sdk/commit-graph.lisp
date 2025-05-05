;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/commit-graph
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/request
                #:request)
  (:import-from #:screenshotbot/sdk/git
                #:null-repo
                #:read-graph
                #:fetch-remote-branch
                #:repo-link)
  #+lispworks
  (:import-from #:screenshotbot/sdk/git-pack
                #:read-commits
                #:make-remote-upload-pack
                #:upload-pack
                #:supported-remote-repo-p)
  (:import-from #:util/misc
                #:?.)
  (:local-nicknames (#:dto #:screenshotbot/api/model)
                    (#:git #:screenshotbot/sdk/git)))
(in-package :screenshotbot/sdk/commit-graph)

(defmethod get-commit-graph-refs (api-context
                                  repo)
  (dto:decode-json
   (request
    api-context
    "/api/commit-graph/refs"
    :method :get
    :decode-response nil
    :parameters (list
                 (cons "repo-url" repo)))
   '(:list dto:git-ref)))

(defun new-flow-enabled-p (repo)
  #+lispworks
  (and
   (?. supported-remote-repo-p (git:get-remote-url repo))
   (str:non-empty-string-p (uiop:getenv "SCREENSHOTBOT_ENABLE_UPLOAD_PACK")))
  #-lispworks
  nil)

(defmethod update-commit-graph-old-style (api-context repo branch)
  "Update commit-graph by pulling, and then always push the top 1000 or
so changes."
  (fetch-remote-branch repo branch)
  (log:info "Updating commit graph")
  (let* ((dag (read-graph repo))
         (json (with-output-to-string (s)
                 (dag:write-to-stream dag s))))
    (request
     api-context
     "/api/commit-graph"
     :method :post
     :parameters (list
                  (cons "repo-url" (repo-link repo))
                  (cons "branch" branch)
                  (cons "graph-json" json)))))

#+lispworks
(defun update-commit-graph-new-style (api-context repo branch)
  (let ((upload-pack (make-remote-upload-pack repo)))
    (update-from-pack
     api-context
     upload-pack
     (git:get-remote-url repo)
     (list branch
           (git:current-branch repo)))))

#+lispworks
(defmethod update-from-pack (api-context
                             (upload-pack upload-pack)
                             (repo-url string)
                             branches)
  (let ((commands (read-commits
                   upload-pack
                   ;; TODO: also do release branches, but that will need a regex here
                   :wants branches)))))


(defmethod update-commit-graph (api-context repo branch)
  (cond
    ((new-flow-enabled-p repo)
     (update-commit-graph-new-style api-context repo branch))
    (t
     (update-commit-graph-old-style api-context repo branch))))

(defmethod update-commit-graph (api-context (repo null-repo) branch)
  (log:info "Not updating the commit graph, since there's no repo"))




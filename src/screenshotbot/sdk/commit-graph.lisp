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
                #:remote-ref-equals
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

(defun ref-in-sync-p (known-refs sha ref)
  "At this point, we know we're interested in this ref. What we need to know is if this is in sync with the server"
  (declare (ignore ref))
  (loop for known-ref in known-refs
        ;; Weird, note that we actually don't care about ref here,
        ;; since if this commit was seen on the server, then that's
        ;; good enough.
        if (equal (dto:git-ref-sha known-ref) sha)
          return t))

#+lispworks
(defun want-remote-ref (known-refs branches sha ref)
  (check-type known-refs list)
  (loop for branch in branches
        if (remote-ref-equals branch ref)
          return (not (ref-in-sync-p known-refs sha ref))))

#+lispworks
(defmethod update-from-pack (api-context
                             (upload-pack upload-pack)
                             (repo-url string)
                             branches)
  (let ((known-refs (get-commit-graph-refs api-context repo-url))
        (refs nil))
    (check-type known-refs list)
    (let ((commits (read-commits
                    upload-pack
                    ;; TODO: also do release branches, but that will need a regex here
                    :wants (lambda (sha ref)
                             (when (str:starts-with-p "refs/heads/" ref)
                               (push (make-instance 'dto:git-ref
                                                    :name (str:substring (length "refs/heads/") nil ref)
                                                    :sha sha)
                                     refs))
                             (want-remote-ref known-refs branches
                                              sha ref))
                    :haves (loop for known-ref in known-refs
                                 collect (dto:git-ref-sha known-ref))
                    :parse-parents t)))
      (let ((commits (loop for (sha . parents) in commits
                           collect (make-instance 'dto:commit
                                                  :sha sha
                                                  :parents parents))))
        (request
         api-context
         "/api/commit-graph"
         :method :post
         :parameters (list
                      (cons "repo-url" repo-url)
                      (cons "graph-json" (dto:encode-json (or commits #())))
                      (cons "refs" (dto:encode-json (or refs #())))))))))


(defmethod update-commit-graph (api-context repo branch)
  (cond
    ((new-flow-enabled-p repo)
     (update-commit-graph-new-style api-context repo branch))
    (t
     (update-commit-graph-old-style api-context repo branch))))

(defmethod update-commit-graph (api-context (repo null-repo) branch)
  (log:info "Not updating the commit graph, since there's no repo"))




